(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

module Cset = Sedlex_cset

(* NFA *)

type tag_op = Set_position of int | Set_value of int * int | Set_prev of int

type node = {
  id : int;
  mutable eps : node list;
  mutable trans : (Cset.t * node) list;
  tag : tag_op option;
}

(* Compilation regexp -> NFA *)

type regexp = node -> node

let cur_id = ref 0

let new_node () =
  incr cur_id;
  { id = !cur_id; eps = []; trans = []; tag = None }

let new_tagged_node tag_op =
  incr cur_id;
  { id = !cur_id; eps = []; trans = []; tag = Some tag_op }

let seq r1 r2 succ = r1 (r2 succ)

let is_chars final = function
  | { eps = []; trans = [(c, f)]; tag = None; _ } when f == final -> Some c
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

(* Tags for as-bindings *)

let cur_tag = ref 0
let reset_tags () = cur_tag := 0

let new_tag () =
  let t = !cur_tag in
  incr cur_tag;
  t

let bind r =
  let start_tag = new_tag () in
  let end_tag = new_tag () in
  let wrapped succ =
    let end_node = new_tagged_node (Set_position end_tag) in
    end_node.eps <- [succ];
    let inner = r end_node in
    let start_node = new_tagged_node (Set_position start_tag) in
    start_node.eps <- [inner];
    start_node
  in
  (wrapped, start_tag, end_tag)

let bind_start_only r =
  let start_tag = new_tag () in
  let wrapped succ =
    let inner = r succ in
    let start_node = new_tagged_node (Set_position start_tag) in
    start_node.eps <- [inner];
    start_node
  in
  (wrapped, start_tag)

let bind_end_only r =
  let end_tag = new_tag () in
  let wrapped succ =
    let end_node = new_tagged_node (Set_position end_tag) in
    end_node.eps <- [succ];
    r end_node
  in
  (wrapped, end_tag)

let new_disc_cell () = new_tag ()

let bind_disc r cell value =
  let wrapped succ =
    let disc_node = new_tagged_node (Set_value (cell, value)) in
    disc_node.eps <- [succ];
    r disc_node
  in
  wrapped

let compile_re re =
  let final = new_node () in
  (re final, final)

(* Determinization *)

type state = node list
(* A state of the DFA corresponds to a set of nodes in the NFA. *)

let rec add_node (state, tags) node =
  if List.memq node state then (state, tags)
  else (
    let tags = match node.tag with Some op -> op :: tags | None -> tags in
    add_nodes (node :: state, tags) node.eps)

and add_nodes acc nodes = List.fold_left add_node acc nodes

(* When multiple Set_value ops target the same cell (because both branches
   of an alt are simultaneously active in a DFA state), keep only the one
   with the lowest value — this gives first-branch-wins semantics. *)
let dedup_tags tags =
  let dominated = Hashtbl.create 4 in
  List.iter
    (function
      | Set_value (cell, value) -> (
          match Hashtbl.find_opt dominated cell with
            | Some v when v <= value -> ()
            | _ -> Hashtbl.replace dominated cell value)
      | Set_position _ | Set_prev _ -> ())
    tags;
  List.filter
    (function
      | Set_value (cell, value) -> Hashtbl.find dominated cell = value
      | Set_position _ | Set_prev _ -> true)
    tags

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

  (* Epsilon closure of targets, collecting tags *)
  let t =
    List.map
      (fun (c, ns) ->
        let state, tags = add_nodes ([], []) ns in
        (c, state, dedup_tags tags))
      t
  in

  (* Canonical ordering *)
  let t = Array.of_list t in
  Array.sort (fun (c1, _, _) (c2, _, _) -> compare c1 c2) t;
  t

type dfa_state = {
  trans : (Cset.t * int * tag_op list) array;
  finals : bool array;
}

type dfa = dfa_state array

type compiled = {
  dfa : dfa;
  init_tags : tag_op list;
  num_tags : int;
  tag_map : int array;
}

let compile rs =
  let rs = Array.map compile_re rs in
  let num_tags_raw = !cur_tag in
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
      let trans = Array.map (fun (p, t, tags) -> (p, aux t, tags)) trans in
      let finals = Array.map (fun (_, f) -> List.memq f state) rs in
      Hashtbl.add states_def i (trans, finals);
      i
  in
  let init = ref ([], []) in
  Array.iter (fun (i, _) -> init := add_node !init i) rs;
  let init_state, init_tags = !init in
  let i = aux init_state in
  assert (i = 0);
  let num_states = !counter in
  let raw_dfa = Array.init num_states (Hashtbl.find states_def) in
  if num_tags_raw = 0 then (
    let dfa =
      Array.map
        (fun (trans, finals) ->
          {
            trans = Array.map (fun (cs, target, _) -> (cs, target, [])) trans;
            finals;
          })
        raw_dfa
    in
    { dfa; init_tags = []; num_tags = 0; tag_map = [||] })
  else (
    let tag_cell = function
      | Set_position t | Set_value (t, _) -> t
      | Set_prev _ -> assert false
    in
    let num_tags = num_tags_raw in
    let tag_map = Array.init num_tags_raw Fun.id in
    (* Cross-rule cell sharing: non-interfering cells from different rules
       can share the same physical memory slot. *)
    let num_tags, tag_map, raw_dfa, init_tags =
      if num_tags <= 1 || Array.length rs <= 1 then
        (num_tags, tag_map, raw_dfa, init_tags)
      else (
        let tag_to_rule = Array.make num_tags_raw (-1) in
        Array.iteri
          (fun r (start, _) ->
            let visited = Hashtbl.create 31 in
            let rec visit node =
              if not (Hashtbl.mem visited node.id) then (
                Hashtbl.add visited node.id ();
                (match node.tag with
                  | Some op -> tag_to_rule.(tag_cell op) <- r
                  | None -> ());
                List.iter visit node.eps;
                List.iter (fun (_, n) -> visit n) node.trans)
            in
            visit start)
          rs;
        let cell_to_rule = Array.make num_tags (-1) in
        for t = 0 to num_tags_raw - 1 do
          if tag_to_rule.(t) >= 0 then (
            let c = tag_map.(t) in
            let r = tag_to_rule.(t) in
            if cell_to_rule.(c) = -1 then cell_to_rule.(c) <- r
            else if cell_to_rule.(c) <> r then cell_to_rule.(c) <- -2)
        done;
        (* Forward reachability from each cell's write transitions *)
        let fwd = Array.init num_tags (fun _ -> Array.make num_states false) in
        for s = 0 to num_states - 1 do
          let trans, _ = raw_dfa.(s) in
          Array.iter
            (fun (_, target, tags) ->
              List.iter (fun op -> fwd.(tag_cell op).(target) <- true) tags)
            trans
        done;
        List.iter (fun op -> fwd.(tag_cell op).(0) <- true) init_tags;
        for c = 0 to num_tags - 1 do
          let queue = Queue.create () in
          for s = 0 to num_states - 1 do
            if fwd.(c).(s) then Queue.push s queue
          done;
          while not (Queue.is_empty queue) do
            let s = Queue.pop queue in
            let trans, _ = raw_dfa.(s) in
            Array.iter
              (fun (_, target, _) ->
                if not fwd.(c).(target) then (
                  fwd.(c).(target) <- true;
                  Queue.push target queue))
              trans
          done
        done;
        (* Greedy graph coloring: cells interfere if same rule, multi-rule,
           or overlapping forward-reachable sets *)
        let slot_of = Array.make num_tags 0 in
        let max_slot = ref 0 in
        for c = 0 to num_tags - 1 do
          let used = Hashtbl.create 8 in
          for c' = 0 to c - 1 do
            let conflict =
              cell_to_rule.(c) < 0
              || cell_to_rule.(c') < 0
              || cell_to_rule.(c) = cell_to_rule.(c')
              ||
              let overlap = ref false in
              for s = 0 to num_states - 1 do
                if fwd.(c).(s) && fwd.(c').(s) then overlap := true
              done;
              !overlap
            in
            if conflict then Hashtbl.replace used slot_of.(c') ()
          done;
          let slot = ref 0 in
          while Hashtbl.mem used !slot do
            incr slot
          done;
          slot_of.(c) <- !slot;
          if !slot > !max_slot then max_slot := !slot
        done;
        let num_slots = !max_slot + 1 in
        if num_slots < num_tags then (
          let tag_map = Array.map (fun c -> slot_of.(c)) tag_map in
          let remap_slot = function
            | Set_position t -> Set_position slot_of.(t)
            | Set_value (c, v) -> Set_value (slot_of.(c), v)
            | Set_prev _ -> assert false
          in
          let raw_dfa =
            Array.map
              (fun (trans, finals) ->
                let trans =
                  Array.map
                    (fun (cs, target, tags) ->
                      let tags =
                        List.sort_uniq compare (List.map remap_slot tags)
                      in
                      (cs, target, tags))
                    trans
                in
                (trans, finals))
              raw_dfa
          in
          let init_tags =
            List.sort_uniq compare (List.map remap_slot init_tags)
          in
          (num_slots, tag_map, raw_dfa, init_tags))
        else (num_tags, tag_map, raw_dfa, init_tags))
    in
    (* Self-loop tag delay: tags on a self-loop s→s that also appear on ALL
       entering transitions to s are removed and set as Set_prev on exit. *)
    let delayed_at = Array.make num_states [] in
    for s = 0 to num_states - 1 do
      let trans, _ = raw_dfa.(s) in
      let sl_tags = ref [] in
      Array.iter
        (fun (_, target, tags) -> if target = s then sl_tags := tags)
        trans;
      match !sl_tags with
        | [] -> ()
        | sl ->
            let entering = ref [] in
            for s' = 0 to num_states - 1 do
              if s' <> s then (
                let trans', _ = raw_dfa.(s') in
                Array.iter
                  (fun (_, target, tags) ->
                    if target = s then entering := tags :: !entering)
                  trans')
            done;
            if !entering <> [] then
              delayed_at.(s) <-
                List.filter (fun t -> List.for_all (List.mem t) !entering) sl
    done;
    let dfa =
      Array.mapi
        (fun s (trans, finals) ->
          let trans =
            Array.map
              (fun (cs, target, tags) ->
                let tags =
                  List.filter
                    (fun t -> not (List.mem t delayed_at.(target)))
                    tags
                in
                let delayed_ops =
                  if target <> s then
                    List.filter_map
                      (fun op ->
                        match op with
                          | Set_position t -> Some (Set_prev t)
                          | _ -> None)
                      delayed_at.(s)
                  else []
                in
                (cs, target, tags @ delayed_ops))
              trans
          in
          { trans; finals })
        raw_dfa
    in
    { dfa; init_tags = dedup_tags init_tags; num_tags; tag_map })

let cset_to_label cset =
  let escape_dot c =
    match c with
      | '"' -> "\\\""
      | '\\' -> "\\\\"
      | '<' -> "\\<"
      | '>' -> "\\>"
      | _ -> String.make 1 c
  in
  let format_interval (lo, hi) =
    if lo = -1 && hi = -1 then "EOF"
    else if lo = hi then
      if lo >= 32 && lo <= 126 then "'" ^ escape_dot (Char.chr lo) ^ "'"
      else Printf.sprintf "U+%04X" lo
    else if lo >= 32 && lo <= 126 && hi >= 32 && hi <= 126 then
      "'" ^ escape_dot (Char.chr lo) ^ "'-'" ^ escape_dot (Char.chr hi) ^ "'"
    else Printf.sprintf "U+%04X-U+%04X" lo hi
  in
  String.concat ", "
    (List.map format_interval (cset : Cset.t :> (int * int) list))

let dfa_to_dot dfa =
  let buf = Buffer.create 1024 in
  let bprintf = Printf.bprintf in
  bprintf buf "digraph {\n";
  bprintf buf "  rankdir=LR;\n";
  bprintf buf "  node [shape=circle];\n\n";
  bprintf buf "  _start [shape=point];\n";
  bprintf buf "  _start -> state0;\n\n";
  Array.iteri
    (fun i { trans; finals } ->
      let accepted =
        let acc = ref [] in
        for r = Array.length finals - 1 downto 0 do
          if finals.(r) then acc := r :: !acc
        done;
        !acc
      in
      (match accepted with
        | [] -> bprintf buf "  state%d [label=\"%d\"];\n" i i
        | rules ->
            bprintf buf
              "  state%d [label=\"%d\\n[rule %s]\", shape=doublecircle];\n" i i
              (String.concat "," (List.map string_of_int rules)));
      Array.iter
        (fun (cset, target, tags) ->
          let label = cset_to_label cset in
          let tag_op_to_string = function
            | Set_position t -> "t" ^ string_of_int t
            | Set_value (c, v) -> "d" ^ string_of_int c ^ "=" ^ string_of_int v
            | Set_prev t -> "t" ^ string_of_int t ^ "'"
          in
          let label =
            if tags = [] then label
            else
              label ^ " {"
              ^ String.concat "," (List.map tag_op_to_string tags)
              ^ "}"
          in
          bprintf buf "  state%d -> state%d [label=\"%s\"];\n" i target label)
        trans)
    dfa;
  bprintf buf "}\n";
  Buffer.contents buf
