(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

(* NFA *)

type node = {
  id : int;
  mutable eps : node list;
  mutable trans : (Cset.t * node) list;
}

(* Compilation regexp -> NFA *)

type regexp = node -> node

let cur_id = ref 0
let new_node () =
  incr cur_id;
  { id = !cur_id; eps = []; trans = [] }

let seq r1 r2 succ = r1 (r2 succ)

let alt r1 r2 succ =
  let n = new_node () in
  n.eps <- [r1 succ; r2 succ];
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

let eps succ = succ

let chars c succ =
  let n = new_node () in
  n.trans <- [c,succ];
  n

let compile_re re =
  let final = new_node () in
  (re final, final)

(* Determinization *)

type state = node list
      (* A state of the DFA corresponds to a set of nodes in the NFA. *)

let rec add_node state node =
  if List.memq node state then state else add_nodes (node::state) node.eps
and add_nodes state nodes =
  List.fold_left add_node state nodes


let transition (state : state) =
  (* Merge transition with the same target *)
  let rec norm = function
    | (c1, n1)::((c2, n2)::q as l) ->
	if n1 == n2 then norm ((Cset.union c1 c2, n1)::q)
	else (c1, n1)::(norm l)
    | l -> l in
  let t = List.concat (List.map (fun n -> n.trans) state) in
  let t = norm (List.sort (fun (_, n1) (_, n2) -> n1.id - n2.id) t) in

  (* Split char sets so as to make them disjoint *)
  let split (all, t) (c0, n0) =
    let t =
      (Cset.difference c0 all, [n0]) ::
      List.map (fun (c, ns) -> (Cset.intersection c c0, n0::ns)) t @
      List.map (fun (c, ns) -> (Cset.difference c c0, ns)) t
    in
    Cset.union all c0,
    List.filter (fun (c, _) -> not (Cset.is_empty c)) t
  in

  let (_,t) = List.fold_left split (Cset.empty,[]) t in

  (* Epsilon closure of targets *)
  let t = List.map (fun (c, ns) -> (c, add_nodes [] ns)) t in

  (* Canonical ordering *)
  let t = Array.of_list t in
  Array.sort (fun (c1, _) (c2, _) -> compare c1 c2) t;
  t

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
  Array.iter (fun (i,_) -> init := add_node !init i) rs;
  let i = aux !init in
  assert(i = 0);
  Array.init !counter (Hashtbl.find states_def)
