open Ast
open Elang
open Prog
open Report
open Options
open Batteries
open Elang_print
open Utils

let tag_is_binop =
  function
    Tadd -> true
  | Tsub -> true
  | Tmul -> true
  | Tdiv -> true
  | Tmod -> true
  | Txor -> true
  | Tcle -> true
  | Tclt -> true
  | Tcge -> true
  | Tcgt -> true
  | Tceq -> true
  | Tne  -> true
  | _    -> false

let binop_of_tag =
  function
    Tadd -> Eadd
  | Tsub -> Esub
  | Tmul -> Emul
  | Tdiv -> Ediv
  | Tmod -> Emod
  | Txor -> Exor
  | Tcle -> Ecle
  | Tclt -> Eclt
  | Tcge -> Ecge
  | Tcgt -> Ecgt
  | Tceq -> Eceq
  | Tne -> Ecne
  | _ -> assert false

(* [make_eexpr_of_ast a] builds an expression corresponding to a tree [a]. If
   the tree is not well-formed, fails with an [Error] message. *)
let rec make_eexpr_of_ast (a: tree) : expr res =
  let res =
    match a with
    | Node (t, [e1; e2]) when tag_is_binop t ->
      make_eexpr_of_ast e1 >>= fun e1 ->
      make_eexpr_of_ast e2 >>= fun e2 ->
      OK (Ebinop (binop_of_tag t, e1, e2))
    | Node (Tneg, [e]) ->
      make_eexpr_of_ast e >>= fun e ->
      OK (Eunop (Eneg, e))
    | Node (Tint, [IntLeaf i]) ->
      OK (Eint i)
    | StringLeaf s ->
      OK (Evar s)
    | _ -> Error (Printf.sprintf "Unacceptable ast in make_eexpr_of_ast %s"
                    (string_of_ast a))
  in
  match res with
    OK o -> res
  | Error msg -> Error (Format.sprintf "In make_eexpr_of_ast %s:\n%s"
                          (string_of_ast a) msg)

let rec make_einstr_of_ast (a: tree) : instr res =
  let res =
    match a with
    | Node (Tassign, [Node(Tassignvar, [StringLeaf s; e])]) ->
      make_eexpr_of_ast e >>= fun e ->
      OK (Iassign (s, e))
    | Node (Tif, [e; i1; i2]) ->
      make_eexpr_of_ast e >>= fun e ->
      make_einstr_of_ast i1 >>= fun i1 ->
      make_einstr_of_ast i2 >>= fun i2 ->
      OK (Iif (e, i1, i2))
    | Node (Twhile, [e; i]) ->
      make_eexpr_of_ast e >>= fun e ->
      make_einstr_of_ast i >>= fun i ->
      OK (Iwhile (e, i))
    | Node (Tblock, inst_list) ->
      list_map_res make_einstr_of_ast inst_list >>= fun inst_list ->
      OK (Iblock inst_list)
    | Node (Treturn, [e]) ->
      make_eexpr_of_ast e >>= fun e ->
      OK (Ireturn e)
    | Node (Tprint, [e]) ->
      make_eexpr_of_ast e >>= fun e ->
      OK (Iprint e)
    | _ -> Error (Printf.sprintf "Unacceptable ast in make_einstr_of_ast %s"
                    (string_of_ast a))
  in
  match res with
    OK o -> res
  | Error msg -> Error (Format.sprintf "In make_einstr_of_ast %s:\n%s"
                          (string_of_ast a) msg)

let make_ident (a: tree) : string res =
  match a with
  | Node (Targ, [s]) ->
    OK (string_of_stringleaf s)
  | a -> Error (Printf.sprintf "make_ident: unexpected AST: %s"
                  (string_of_ast a))

let make_fundef_of_ast (a: tree) : (string * efun) res =
  match a with
  | Node (Tfundef, [Node (Tfunname, [StringLeaf(fname)]); Node(Tfunargs, fargs); Node(Tfunbody, [fbody])]) ->
    list_map_res make_ident fargs >>= fun fargs ->
    make_einstr_of_ast fbody >>= fun fbody ->
      let func: efun = {funargs = fargs; funbody = fbody} in
    OK (fname, func)
  | _ ->
    Error (Printf.sprintf "make_fundef_of_ast: Expected a Tfundef, got %s."
             (string_of_ast a))

let make_eprog_of_ast (a: tree) : eprog res =
  match a with
  | Node (Tlistglobdef, l) ->
    list_map_res (fun a -> make_fundef_of_ast a >>= fun (fname, efun) -> OK (fname, Gfun efun)) l
  | _ ->
    Error (Printf.sprintf "make_fundef_of_ast: Expected a Tlistglobdef, got %s."
             (string_of_ast a))

let pass_elang ast =
  match make_eprog_of_ast ast with
  | Error msg ->
    record_compile_result ~error:(Some msg) "Elang";
    Error msg
  | OK  ep ->
    dump !e_dump dump_e ep (fun file () ->
        add_to_report "e" "E" (Code (file_contents file))); OK ep

