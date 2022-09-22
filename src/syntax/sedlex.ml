(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

module Cset = Sedlex_cset

(* NFA *)

type node = {
  id : int;
  mutable eps : node list;
  mutable trans : (Cset.t * node) list;
  mutable alias : (string * bool) option;
}

(* Compilation regexp -> NFA *)

type regexp = node -> node

let cur_id = ref 0

let new_node () =
  incr cur_id;
  { id = !cur_id; eps = []; trans = []; alias = None }

let seq r1 r2 succ = r1 (r2 succ)

let is_chars final = function
  | { eps = []; trans = [(c, f)] } when f == final -> Some c
  | _ -> None

let chars c succ =
  let n = new_node () in
  n.trans <- [(c, succ)];
  n

let alt r1 r2 succ =
  let nr1 = r1 succ and nr2 = r2 succ in
  match (is_chars succ nr1, is_chars succ nr2) with
    | Some c1, Some c2 -> chars (Cset.union c1 c2) succ
    | _ ->
        let n = new_node () in
        n.eps <- [nr1; nr2];
        n

let rep r succ =
  let n = new_node () in
  n.eps <- [r n; succ];
  n

let plus r succ =
  let n = new_node () in
  let nr = r n in
  n.eps <- [nr; succ];
  nr

let eps succ = succ (* eps for epsilon *)

let compl r =
  let n = new_node () in
  match is_chars n (r n) with
    | Some c -> Some (chars (Cset.difference Cset.any c))
    | _ -> None

let pair_op f r0 r1 =
  (* Construct subtract or intersection *)
  let n = new_node () in
  let to_chars r = is_chars n (r n) in
  match (to_chars r0, to_chars r1) with
    | Some c0, Some c1 -> Some (chars (f c0 c1))
    | _ -> None

let subtract = pair_op Cset.difference
let intersection = pair_op Cset.intersection

let alias r alias succ =
  let n = new_node () in
  let s = new_node () in
  s.eps <- [succ];
  s.alias <- Some (alias, false);
  n.alias <- Some (alias, true);
  n.eps <- [r s];
  n

let compile_re re =
  let final = new_node () in
  (re final, final)

(* Determinization *)

type state = node list
(* A state of the DFA corresponds to a set of nodes in the NFA. *)

let rec add_node state node =
  if List.memq node state then state else add_nodes (node :: state) node.eps

and add_nodes state nodes = List.fold_left add_node state nodes

let transition (state : state) =
  (* Merge transition with the same target *)
  let rec norm = function
    | (c1, n1) :: ((c2, n2) :: q as l) ->
        if n1 == n2 then norm ((Cset.union c1 c2, n1) :: q)
        else (c1, n1) :: norm l
    | l -> l
  in
  let t = List.concat (List.map (fun n -> n.trans) state) in
  let t = norm (List.sort (fun (_, n1) (_, n2) -> n1.id - n2.id) t) in

  (* Split char sets so as to make them disjoint *)
  let split (all, t) (c0, n0) =
    let t =
      (Cset.difference c0 all, [n0])
      :: List.map (fun (c, ns) -> (Cset.intersection c c0, n0 :: ns)) t
      @ List.map (fun (c, ns) -> (Cset.difference c c0, ns)) t
    in
    (Cset.union all c0, List.filter (fun (c, _) -> not (Cset.is_empty c)) t)
  in

  let _, t = List.fold_left split (Cset.empty, []) t in

  (* Epsilon closure of targets *)
  let t = List.map (fun (c, ns) -> (c, add_nodes [] ns)) t in

  (* Canonical ordering *)
  let t = Array.of_list t in
  Array.sort (fun (c1, _) (c2, _) -> compare c1 c2) t;
  t

let compile_traces states (start, final) =
  let counter = ref 0 in
  let nodes_idx = Hashtbl.create 31 in
  let rec aux node =
    try ignore (Hashtbl.find nodes_idx node)
    with Not_found ->
      let i = !counter in
      incr counter;
      Hashtbl.add nodes_idx node i;
      List.iter aux node.eps;
      List.iter (fun (_, next) -> aux next) node.trans
  in
  aux start;
  let handle_alias (starts, stops) = function
    | None -> (starts, stops)
    | Some (alias, flag) ->
        if flag then (alias :: starts, stops) else (starts, alias :: stops)
  in
  let first_node = Hashtbl.find nodes_idx final in
  let trans_cases =
    let cases = Hashtbl.create 31 in
    Hashtbl.iter
      (fun from_state j ->
        Hashtbl.iter
          (fun to_state i ->
            List.iter
              (fun from_node ->
                try
                  let node_j = Hashtbl.find nodes_idx from_node in
                  let rec dfs start_stops to_node =
                    let node_i = Hashtbl.find nodes_idx to_node in
                    try ignore (Hashtbl.find cases (i, node_i, j))
                    with Not_found ->
                      let starts, stops =
                        handle_alias start_stops to_node.alias
                      in
                      Hashtbl.add cases (i, node_i, j)
                        (i, node_i, j, node_j, starts, stops);
                      List.iter (dfs (starts, stops)) to_node.eps
                  in
                  List.iter
                    (fun (_, to_node) ->
                      if List.mem to_node to_state then dfs ([], []) to_node)
                    from_node.trans
                with Not_found -> ())
              from_state)
          states)
      states;
    Hashtbl.to_seq_values cases |> List.of_seq
  in
  let final_cases =
    let rec dfs start_stops cases node =
      let i = Hashtbl.find nodes_idx node in
      let starts, stops = handle_alias start_stops node.alias in
      let cases = (i, starts, stops) :: cases in
      List.fold_left (dfs (starts, stops)) cases node.eps
    in
    dfs ([], []) [] start
  in
  (first_node, trans_cases, final_cases)

let compile rs =
  let rs = Array.map compile_re rs in
  let counter = ref 0 in
  let states = Hashtbl.create 31 in
  let states_def = Hashtbl.create 31 in
  let rec aux state =
    try Hashtbl.find states state
    with Not_found ->
      let i = !counter in
      incr counter;
      Hashtbl.add states state i;
      let trans = transition state in
      let trans = Array.map (fun (p, t) -> (p, aux t)) trans in
      let finals = Array.map (fun (_, f) -> List.memq f state) rs in
      Hashtbl.add states_def i (trans, finals);
      i
  in
  let init = ref [] in
  Array.iter (fun (i, _) -> init := add_node !init i) rs;
  let i = aux !init in
  assert (i = 0);
  ( Array.init !counter (Hashtbl.find states_def),
    Array.map (compile_traces states) rs )
