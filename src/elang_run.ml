open Elang
open Batteries
open Prog
open Utils
open Builtins

let binop_bool_to_int f x y = if f x y then 1 else 0

(* [eval_binop b x y] évalue l'opération binaire [b] sur les arguments [x]
   et [y]. *)
let eval_binop (b: binop) : int -> int -> int =
  match b with
  | Eadd -> (+)
  | Emul -> ( * )
  | Emod -> (mod)
  | Exor -> (lxor)
  | Ediv -> (/)
  | Esub -> (-)
  | Eclt -> binop_bool_to_int (<)
  | Ecle -> binop_bool_to_int (<=)
  | Ecgt -> binop_bool_to_int (>)
  | Ecge -> binop_bool_to_int (>=)
  | Eceq -> binop_bool_to_int (=)
  | Ecne -> binop_bool_to_int (<>)

(* [eval_unop u x] évalue l'opération unaire [u] sur l'argument [x]. *)
let eval_unop (u: unop) : int -> int =
  match u with
  | Eneg -> fun x -> -x

(* [eval_eexpr st e] évalue l'expression [e] dans l'état [st]. Renvoie une
   erreur si besoin. *)
let rec eval_eexpr ep oc st (e : expr) : int res =
  match e with
  | Ebinop (b, e1, e2) ->
    eval_eexpr ep oc st e1 >>= fun v1 ->
    eval_eexpr ep oc st e2 >>= fun v2 ->
    OK (eval_binop b v1 v2)
  | Eunop (u, e) ->
    eval_eexpr ep oc st e >>= fun v ->
    OK (eval_unop u v)
  | Eint n -> OK n
  | Evar x ->
    (match Hashtbl.find_option st.env x with
     | Some v -> OK v
     | None -> Error (Format.sprintf "E: Variable %s not found.\n" x)
    )
    | Ecall (fname,args) -> 
      find_function ep fname >>! (fun f ->
      eval_efun ep oc st f fname (List.map (fun arg -> eval_eexpr ep oc st arg >>! fun valeur -> valeur) args)
       >>! (fun (fun_result, new_state) ->  
      (match fun_result with 
      | Some result -> OK (result)
      | None -> Error ("On attend une valeur de retour pour cet appel de fonction"))))

    
(* [eval_einstr oc st ins] évalue l'instrution [ins] en partant de l'état [st].

   Le paramètre [oc] est un "output channel", dans lequel la fonction "print"
   écrit sa sortie, au moyen de l'instruction [Format.fprintf].

   Cette fonction renvoie [(ret, st')] :

   - [ret] est de type [int option]. [Some v] doit être renvoyé lorsqu'une
     instruction [return] est évaluée. [None] signifie qu'aucun [return] n'a eu
     lieu et que l'exécution doit continuer.

   - [st'] est l'état mis à jour. *)
and eval_einstr ep oc (st: int state) (ins: instr) :
  (int option * int state) res =
  match ins with
  | Iassign (x, e) ->
    eval_eexpr ep oc st e >>= fun v ->
    Hashtbl.replace st.env x v;
    OK (None, st)
  | Iif (e, i1, i2) ->
    eval_eexpr ep oc st e >>= fun v ->
    if v <> 0 then
      eval_einstr ep oc st i1
    else
      eval_einstr ep oc st i2
  | Iwhile (e, i) ->
    eval_eexpr ep oc st e >>= fun v ->
    if v <> 0 then
      eval_einstr ep  oc st i >>= fun (ret, st') ->
      if ret <> None then
        OK (ret, st')
      else
        eval_einstr ep oc st' ins
    else
      OK (None, st)
  | Iblock il ->
    List.fold_left (fun acc i ->
        acc >>= fun (ret, st') ->
        if ret <> None then
          OK (ret, st')
        else
          eval_einstr ep oc st' i
      ) (OK (None, st)) il
  | Ireturn e ->
    eval_eexpr ep oc st e >>= fun v ->
    OK (Some v, st)
    | Icall (funame,args) -> 
      match do_builtin oc st.mem funame (List.map (fun arg -> eval_eexpr ep oc st arg >>! fun valeur -> valeur) args) with
      | OK Some i -> OK (Some i,st)
      | OK None -> OK (None, st)
      | Error _ -> 
         find_function ep funame >>! (fun f ->
            eval_efun ep oc st f funame (List.map (fun arg -> eval_eexpr ep oc st arg >>! fun valeur -> valeur) args) >>! 
            (fun (fun_result, new_state) -> 
               OK (None,new_state)
                  ))
   

(* [eval_efun oc st f fname vargs] évalue la fonction [f] (dont le nom est
   [fname]) en partant de l'état [st], avec les arguments [vargs].

   Cette fonction renvoie un couple (ret, st') avec la même signification que
   pour [eval_einstr]. *)
and eval_efun ep oc (st: int state) ({ funargs; funbody}: efun)
    (fname: string) (vargs: int list)
  : (int option * int state) res =
  (* L'environnement d'une fonction (mapping des variables locales vers leurs
     valeurs) est local et un appel de fonction ne devrait pas modifier les
     variables de l'appelant. Donc, on sauvegarde l'environnement de l'appelant
     dans [env_save], on appelle la fonction dans un environnement propre (Avec
     seulement ses arguments), puis on restore l'environnement de l'appelant. *)
  let env_save = Hashtbl.copy st.env in
  let env = Hashtbl.create 17 in
  match List.iter2 (fun a v -> Hashtbl.replace env a v) funargs vargs with
  | () ->
    eval_einstr ep oc { st with env } funbody >>= fun (v, st') ->
    OK (v, { st' with env = env_save })
  | exception Invalid_argument _ ->
    Error (Format.sprintf
             "E: Called function %s with %d arguments, expected %d.\n"
             fname (List.length vargs) (List.length funargs)
          )

(* [eval_eprog oc ep memsize params] évalue un programme complet [ep], avec les
   arguments [params].

   Le paramètre [memsize] donne la taille de la mémoire dont ce programme va
   disposer. Ce n'est pas utile tout de suite (nos programmes n'utilisent pas de
   mémoire), mais ça le sera lorsqu'on ajoutera de l'allocation dynamique dans
   nos programmes.

   Renvoie:

   - [OK (Some v)] lorsque l'évaluation de la fonction a lieu sans problèmes et renvoie une valeur [v].

   - [OK None] lorsque l'évaluation de la fonction termine sans renvoyer de valeur.

   - [Error msg] lorsqu'une erreur survient.
*)
let eval_eprog oc (ep: eprog) (memsize: int) (params: int list)
  : int option res =
  let st = init_state memsize in
  find_function ep "main" >>= fun f ->
  (* ne garde que le nombre nécessaire de paramètres pour la fonction "main". *)
  let n = List.length f.funargs in
  let params = take n params in
  eval_efun ep oc st f "main" params >>= fun (v, _) ->
  OK v
