open Prog
open Elang
open Elang_run
open Batteries
open BatList
open Cfg
open Utils
open Builtins

let rec eval_cfgexpr cp oc st (e: expr) : int res =
  match e with
  | Ebinop(b, e1, e2) ->
    eval_cfgexpr cp oc st e1 >>= fun v1 ->
    eval_cfgexpr cp oc st e2 >>= fun v2 ->
    let v = eval_binop b v1 v2 in
    OK v
  | Eunop(u, e) ->
    eval_cfgexpr cp oc st e >>= fun v1 ->
    let v = (eval_unop u v1) in
    OK v
  | Eint i -> OK i
  | Evar s ->
    begin match Hashtbl.find_option st.env s with
      | Some v -> OK v
      | None -> Error (Printf.sprintf "Unknown variable %s\n" s)
    end
  | Ecall (fname, args) -> 
      find_function cp fname >>! fun f -> (
      eval_cfgfun cp oc st fname f (List.map (fun arg -> eval_cfgexpr cp oc st arg >>! fun valeur -> valeur) args)
      >>!(fun (fun_result, new_state) ->  
        (match fun_result with 
        | Some result -> OK (result)
        | None -> Error ("On attend une valeur de retour pour cet appel de fonction"))))

  

and eval_cfginstr cp oc st ht (n: int): (int * int state) res =
  match Hashtbl.find_option ht n with
  | None -> Error (Printf.sprintf "Invalid node identifier\n")
  | Some node ->
    match node with
    | Cnop succ ->
      eval_cfginstr cp oc st ht succ
    | Cassign(v, e, succ) ->
      eval_cfgexpr cp oc  st e >>= fun i ->
      Hashtbl.replace st.env v i;
      eval_cfginstr cp oc st ht succ
    | Ccmp(cond, i1, i2) ->
      eval_cfgexpr cp oc st cond >>= fun i ->
      if i = 0 then eval_cfginstr cp oc st ht i2 else eval_cfginstr cp oc st ht i1
    | Creturn(e) ->
      eval_cfgexpr cp oc st e >>= fun e ->
      OK (e, st)
      | Ccall (funame,args,s) -> 
        let funargs = (List.map (fun arg -> eval_cfgexpr cp oc st arg >>! fun valeur -> valeur) args) in
        match do_builtin oc st.mem funame funargs with
        | OK Some i -> Error "On n'attend pas de retour de fonction ici"
        | OK None -> eval_cfginstr cp oc st ht s
        | Error _ -> 
          find_function cp funame >>! (fun f ->
            eval_cfgfun cp oc st funame f funargs >>! 
            (fun (fun_result, new_state) -> 
              eval_cfginstr cp oc st ht s
                  ))
  

and eval_cfgfun cp oc st cfgfunname { cfgfunargs;
                                      cfgfunbody;
                                      cfgentry} vargs =
  let st' = { st with env = Hashtbl.create 17 } in
  match List.iter2 (fun a v -> Hashtbl.replace st'.env a v) cfgfunargs vargs with
  | () -> eval_cfginstr cp oc st' cfgfunbody cfgentry >>= fun (v, st') ->
    OK (Some v, {st' with env = st.env})
  | exception Invalid_argument _ ->
    Error (Format.sprintf "CFG: Called function %s with %d arguments, expected %d.\n"
             cfgfunname (List.length vargs) (List.length cfgfunargs)
          )

let eval_cfgprog oc cp memsize params =
  let st = init_state memsize in
  find_function cp "main" >>= fun f ->
  let n = List.length f.cfgfunargs in
  let params = take n params in
  eval_cfgfun cp oc st "main" f params >>= fun (v, st) ->
  OK v


