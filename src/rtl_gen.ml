open Batteries
open Elang
open Cfg
open Rtl
open Prog
open Utils
open Report
open Rtl_print
open Options

(* Une partie de la génération de RTL consiste à allouer les variables dans des
   pseudo-registres RTL.

   Ces registres sont en nombre illimité donc ce problème est facile.

   Étant donnés :
   - [next_reg], le premier numéro de registre disponible (pas encore alloué à
     une variable)
   - [var2reg], une liste d'associations dont les clés sont des variables et les
     valeurs des numéros de registres
   - [v] un nom de variable (de type [string]),

   [find_var (next_reg, var2reg) v] renvoie un triplet [(r, next_reg, var2reg)]:

   - [r] est le registre RTL associé à la variable [v]
   - [next_reg] est le nouveau premier registre disponible
   - [var2reg] est la nouvelle association nom de variable/registre.

*)
let find_var (next_reg, var2reg) v =
  match List.assoc_opt v var2reg with
  | Some r -> (r, next_reg, var2reg)
  | None -> (next_reg, next_reg + 1, assoc_set var2reg v next_reg)

(* [rtl_instrs_of_cfg_expr (next_reg, var2reg) e] construit une liste
   d'instructions RTL correspondant à l'évaluation d'une expression E.

   Le retour de cette fonction est un quadruplet [(r,l,next_reg,var2reg)], où :
   - [r] est le registre RTL dans lequel le résultat de l'évaluation de [e] aura
     été stocké
   - [l] est une liste d'instructions RTL.
   - [next_reg] est le nouveau premier registre disponible
   - [var2reg] est la nouvelle association nom de variable/registre.
*)
let rec rtl_instrs_of_cfg_expr (next_reg, var2reg) (e: expr) =
  match e with
  | Eint(i) -> 
    (next_reg, [Rconst(next_reg, i)], next_reg+1, var2reg)
  | Evar(s) -> 
    let (r, next_reg, var2reg) = find_var (next_reg, var2reg) s in 
    (r, [], next_reg, [(s, r)]@var2reg)
  | Eunop(u, e) -> 
    let (r, _, next_reg, var2reg) = rtl_instrs_of_cfg_expr (next_reg, var2reg) e in
    (r, [Runop(u, r, next_reg)], next_reg+1, var2reg)
  | Ebinop(b, e1, e2) ->
    let (r2, l2, next_reg, var2reg) = rtl_instrs_of_cfg_expr (next_reg, var2reg) e2 in 
    let (r1, l1, next_reg, var2reg) = rtl_instrs_of_cfg_expr (next_reg, var2reg) e1 in
    (next_reg, l2@l1@[Rbinop(b, next_reg, r1, r2)], next_reg+1, var2reg)
  | Ecall (fname, args) -> 
    let rargs,instr,next_reg_after_call,var2reg_after_call = List.fold_left (
      fun acc arg -> 
      let (r_list,instr_list,next_rega,var2rega) = acc in 
      let (newr,newinstr,newnext_reg,newvar2reg) = rtl_instrs_of_cfg_expr (next_rega, var2rega) arg in
      (r_list @ [newr],instr_list @ newinstr , newnext_reg, newvar2reg)
    ) ([],[],next_reg, var2reg) args in
    (next_reg_after_call,instr @ [Rcall(Some next_reg_after_call,fname,rargs)],next_reg_after_call +1,var2reg_after_call)


let is_cmp_op =
  function Eclt -> Some Rclt
         | Ecle -> Some Rcle
         | Ecgt -> Some Rcgt
         | Ecge -> Some Rcge
         | Eceq -> Some Rceq
         | Ecne -> Some Rcne
         | _ -> None

let rtl_cmp_of_cfg_expr (e: expr) =
  match e with
  | Ebinop (b, e1, e2) ->
    (match is_cmp_op b with
     | None -> (Rcne, e, Eint 0)
     | Some rop -> (rop, e1, e2))
  | _ -> (Rcne, e, Eint 0)


let rtl_instrs_of_cfg_node ((next_reg:int), (var2reg: (string*int) list)) (c: cfg_node) =
  let (l, next_reg, var2reg) = match c with
    | Cassign (s, e, i) -> 
      let (r, l, next_reg, var2reg) = rtl_instrs_of_cfg_expr (next_reg, var2reg) e in
      let (rs, next_reg, var2reg) = find_var (next_reg, var2reg) s in
      (l @ [Rmov (rs, r); Rjmp(i)], next_reg, var2reg)
    | Creturn (e) -> let (r, l, next_reg, var2reg) = rtl_instrs_of_cfg_expr (next_reg, var2reg) e in
      (l @ [Rret(r)], next_reg, var2reg)
    | Ccmp (e, i1, i2) ->
      let (rop, e1, e2) = rtl_cmp_of_cfg_expr e in
      let (r1, l1, next_reg, var2reg) = rtl_instrs_of_cfg_expr (next_reg, var2reg) e1 in
      let (r2, l2, next_reg, var2reg) = rtl_instrs_of_cfg_expr (next_reg, var2reg) e2 in
      (l1@l2@[Rbranch (rop, r1, r2, i1); Rjmp(i2)], next_reg, var2reg)
    | Cnop (i) -> ([Rjmp(i)], next_reg, var2reg)
    | Ccall (fname, args, next) -> 
      let r,instr,next_reg_after_call,var2reg_after_call = List.fold_left (
        fun acc arg -> 
        let (r_list,instr_list,next_reg,var2reg) = acc in 
        let (newr,newinstr,newnext_reg,newvar2reg) = rtl_instrs_of_cfg_expr (next_reg, var2reg) arg in
        (r_list @ [newr],instr_list @ newinstr , newnext_reg, newvar2reg)
        ) 
        ([],[],next_reg, var2reg) args in
      (instr @ [Rcall(None,fname,r)] @ [Rjmp(next)],next_reg_after_call,var2reg_after_call)  
  in (l, next_reg, var2reg)

let rtl_instrs_of_cfg_fun cfgfunname ({ cfgfunargs; cfgfunbody; cfgentry }: cfg_fun) =
  let (rargs, next_reg, var2reg) =
    List.fold_left (fun (rargs, next_reg, var2reg) a ->
        let (r, next_reg, var2reg) = find_var (next_reg, var2reg) a in
        (rargs @ [r], next_reg, var2reg)
      )
      ([], 0, []) cfgfunargs
  in
  let rtlfunbody = Hashtbl.create 17 in
  let (next_reg, var2reg) = Hashtbl.fold (fun n node (next_reg, var2reg)->
      let (l, next_reg, var2reg) = rtl_instrs_of_cfg_node (next_reg, var2reg) node in
      Hashtbl.replace rtlfunbody n l;
      (next_reg, var2reg)
    ) cfgfunbody (next_reg, var2reg) in
  {
    rtlfunargs = rargs;
    rtlfunentry = cfgentry;
    rtlfunbody;
    rtlfuninfo = var2reg;
  }

let rtl_of_gdef funname = function
    Gfun f -> Gfun (rtl_instrs_of_cfg_fun funname f)

let rtl_of_cfg cp = List.map (fun (s, gd) -> (s, rtl_of_gdef s gd)) cp

let pass_rtl_gen cfg =
  let rtl = rtl_of_cfg cfg in
  dump !rtl_dump dump_rtl_prog rtl
    (fun file () -> add_to_report "rtl" "RTL" (Code (file_contents file)));
  OK rtl
