open Batteries
open Cfg
open Elang_run
open Prog
open Utils
open Report
open Cfg_print
open Options

(* [simple_eval_eexpr e] evaluates an expression [e] with no variables. Raises
   an exception if the expression contains variables. *)
let rec simple_eval_eexpr (e: expr) : int =
   match e with
   | Ebinop(b, e1, e2) ->( 
    match b with
    | Eadd -> simple_eval_eexpr e1 + simple_eval_eexpr e2
    | Esub -> simple_eval_eexpr e1 - simple_eval_eexpr e2
    | Emul -> simple_eval_eexpr e1 * simple_eval_eexpr e2
    | Ediv -> simple_eval_eexpr e1 / simple_eval_eexpr e2
    | Emod -> simple_eval_eexpr e1 mod simple_eval_eexpr e2
    | Exor -> simple_eval_eexpr e1 lxor simple_eval_eexpr e2
    | Eclt -> if simple_eval_eexpr e1 < simple_eval_eexpr e2 then 1 else 0
    | Ecgt -> if simple_eval_eexpr e1 > simple_eval_eexpr e2 then 1 else 0
    | Ecle -> if simple_eval_eexpr e1 <= simple_eval_eexpr e2 then 1 else 0
    | Ecge -> if simple_eval_eexpr e1 >= simple_eval_eexpr e2 then 1 else 0
    | Eceq -> if simple_eval_eexpr e1 = simple_eval_eexpr e2 then 1 else 0
    | Ecne -> if simple_eval_eexpr e1 <> simple_eval_eexpr e2 then 1 else 0
   )
   | Eunop(u, e) ->(
     match u with
    | Eneg -> - simple_eval_eexpr e
   )
   | Eint i -> i
   | Evar _ -> raise (Failure "simple_eval_eexpr: variable")
    | Ecall(_, _) -> raise (Failure "simple_eval_eexpr: function call that may have side effects")

(* If an expression contains variables, we cannot simply evaluate it. *)

(* [has_vars e] indicates whether [e] contains variables. *)
let rec has_vars (e: expr) =
   match e with
   | Ebinop(_, e1, e2) -> has_vars e1 || has_vars e2
   | Eunop(_, e) -> has_vars e
   | Eint _ -> false
   | Evar _ -> true
   | Ecall(_, _) -> true (* TODO: Effets de bords *)

let const_prop_binop b e1 e2 =
  let e = Ebinop (b, e1, e2) in
  if has_vars e
  then e
  else Eint (simple_eval_eexpr e)

let const_prop_unop u e =
  let e = Eunop (u, e) in
  if has_vars e
  then e
  else Eint (simple_eval_eexpr e)


let rec const_prop_expr (e: expr) =
  match e with
  | Ebinop (b, e1, e2) ->
    const_prop_binop b (const_prop_expr e1) (const_prop_expr e2)
  | Eunop (u, e) ->
    const_prop_unop u (const_prop_expr e)
  | Eint(i) -> e
  | Evar s -> e
  | Ecall(s, el) -> e

let constant_propagation_instr (i: cfg_node) : cfg_node =
  match i with
  | Cassign(s, e, i) -> Cassign(s, const_prop_expr e, i)
  | Creturn(e) -> Creturn(const_prop_expr e)
  | Ccmp(e, i1, i2) -> Ccmp(const_prop_expr e, i1, i2)
  | Cnop(i) -> Cnop(i)
  | Ccall(s, el, i) -> Ccall(s, List.map const_prop_expr el, i)

let constant_propagation_fun ({ cfgfunbody; _ } as f: cfg_fun) =
  let ht = Hashtbl.map (fun _ m ->
      constant_propagation_instr m
    ) cfgfunbody in
  { f with cfgfunbody = ht}

let constant_propagation_gdef = function
    Gfun f ->
    Gfun (constant_propagation_fun f)

let constant_propagation p =
  if !Options.no_cfg_constprop
  then p
  else assoc_map constant_propagation_gdef p

let pass_constant_propagation p =
  let cfg = constant_propagation p in
  record_compile_result "Constprop";
  dump (!cfg_dump >*> fun s -> s ^ "1") dump_cfg_prog cfg
    (call_dot "cfg-after-cstprop" "CFG after Constant Propagation");
  OK cfg
