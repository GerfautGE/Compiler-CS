open Batteries
open Cfg

(* Analyse de vivacité *)

(* [vars_in_expr e] renvoie l'ensemble des variables qui apparaissent dans [e]. *)
let rec vars_in_expr (e: expr) =
  match e with
  | Ebinop (_, e1, e2) -> Set.union (vars_in_expr e1) (vars_in_expr e2)
  | Eunop (_, e) -> vars_in_expr e
  | Eint _ -> Set.empty
  | Evar x -> Set.singleton x

(* [live_cfg_node node live_after] renvoie l'ensemble des variables vivantes
   avant un nœud [node], étant donné l'ensemble [live_after] des variables
   vivantes après ce nœud. *)
let live_cfg_node (node: cfg_node) (live_after: string Set.t) =
  match node with
  | Cassign (s, e, i) -> Set.add s (Set.union (vars_in_expr e) live_after)
  | Creturn e -> Set.union (vars_in_expr e) live_after
  | _ -> live_after

(* [live_cfg_nodes cfg lives] calcule l'ensemble des variables vivantes avant
   chaque nœud du CFG [cfg], en itérant [live_cfg_node] jusqu'à ce qu'un point
   fixe soit atteint. *)

(* [live_after_node cfg n] renvoie l'ensemble des variables vivantes après le
   nœud [n] dans un CFG [cfg]. [lives] est l'état courant de l'analyse,
   c'est-à-dire une table dont les clés sont des identifiants de nœuds du CFG et
   les valeurs sont les ensembles de variables vivantes avant chaque nœud. *)
let live_after_node cfg n (lives: (int, string Set.t) Hashtbl.t) : string Set.t =
   Set.fold (fun elt acc -> Set.union acc (match Hashtbl.find_option lives elt with
                                          |None -> Set.empty
                                          |Some(set) -> set)) 
            (succs cfg n) Set.empty 
(* [live_cfg_nodes cfg lives] effectue une itération du calcul de point fixe.

   Cette fonction met à jour l'état de l'analyse [lives] et renvoie un booléen
   qui indique si le calcul a progressé durant cette itération (i.e. s'il existe
   au moins un nœud n pour lequel l'ensemble des variables vivantes avant ce
   nœud a changé). *)
let live_cfg_nodes cfg (lives : (int, string Set.t) Hashtbl.t) =
   Hashtbl.fold (fun n node b ->
      let prev_in = Hashtbl.find_option lives n in
      let out_n = live_after_node cfg n lives in 
      let next_in = live_cfg_node node out_n in
       match prev_in with
      |None -> begin Hashtbl.replace lives n next_in; true end
      |Some(set) -> if Set.equal set next_in
                        then b
                        else begin Hashtbl.replace lives n next_in; true end ) cfg false

(* [live_cfg_fun f] calcule l'ensemble des variables vivantes avant chaque nœud
   du CFG en itérant [live_cfg_nodes] jusqu'à ce qu'un point fixe soit atteint.
*)
let live_cfg_fun (f: cfg_fun) : (int, string Set.t) Hashtbl.t =
  let lives = Hashtbl.create 17 in
  while live_cfg_nodes f.cfgfunbody lives do () done;
   lives
