(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

(*
   Implementation overview
   =======================

   Sedlex compiles regular expressions to Tagged DFAs.

   1. NFA construction (type regexp = node -> node)
      Each regexp combinator (chars, seq, alt, rep, ...) is a function that,
      given a successor node, builds a fragment of NFA and returns its entry
      node. This continuation-passing style makes sequencing natural (seq is
      just function composition) and avoids explicit epsilon nodes for
      concatenation.

   2. Tags for `as` bindings (Laurikari-style)
      NFA nodes may carry a tag operation (Set_position or Set_value).
      [bind] wraps a sub-regexp with start/end tagged epsilon
      nodes so the DFA can record sub-match positions at runtime. When the
      PPX can compute one boundary from a known offset (see [pos_expr] in
      ppx_sedlex.ml), [bind_start_only] or [bind_end_only] is used instead,
      saving a memory cell. Discriminator tags (Set_value) disambiguate
      or-patterns where multiple branches bind the same name.

      After determinization, [compile] applies the self-loop tag delay
      optimization: when a Set_position tag appears on both a self-loop
      and all entering transitions to a state, it is removed from those
      transitions and replaced by Set_position with offset 1 on exit
      transitions. This
      avoids writing to the memory cell on every loop iteration.

   3. Determinization (compile)
      Classic subset construction, extended to handle tags (Laurikari, NFAs
      with Tagged Transitions, 2000).

      Each DFA state is a set of NFA nodes (represented as a list, identified
      by physical identity via memq). DFA states are memoized in a hash table
      keyed by node lists.

      Tags live on epsilon nodes in the NFA, so they are naturally collected
      during epsilon closure. To compute a DFA transition for character
      set [c]: follow all NFA [c]-transitions from nodes in the current DFA
      state, then compute the epsilon closure of the targets. Every tagged
      node visited during closure contributes its tag operation to the
      transition's tag list. Each (target DFA state, tag list) pair becomes
      one DFA transition: "on input [c], execute these tag ops, go to state N."

      In general, tagged determinization must resolve conflicts when multiple
      active NFA paths write different values to the same tag (Laurikari uses
      per-path tag valuations and priority ordering). Sedlex largely avoids
      this: each [as] binding gets unique tag IDs, and [as] is rejected
      inside repetition operators, so no two active NFA paths ever write to
      the same position tag. The one exception is discriminator cells
      (Set_value): both branches of an alt may be simultaneously active in
      a DFA state; [dedup_tags] resolves this by keeping the lowest value
      (first-branch-wins).

   Possible future optimizations (see #175)
   -----------------------------------------

   Tag optimizations for `as` bindings:
   - Intra-rule tag coalescing: tags with identical occurrence signatures
     (same presence in init_tags and same set of transitions) can share a
     single memory cell.

   DFA construction:
   - DFA minimization: the generated DFA is not minimized. Hopcroft's or
     Moore's algorithm could reduce state count, especially for patterns with
     many character classes that converge to the same accepting state.
*)

module Cset = Sedlex_cset

(* NFA *)

type tag_op =
  | Set_position of { cell : int; offset : int }
  | Set_value of int * int

type node = {
  id : int;  (** Unique identifier, used for sorting transitions by target. *)
  mutable eps : node list;  (** Epsilon successors (no input consumed). *)
  mutable trans : (Cset.t * node) list;  (** Char-set-labelled transitions. *)
  tag : tag_op option;  (** Tag operation executed when entering this node. *)
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

(* [is_chars final node] tests whether [node] is a simple character-set
   node: no epsilon edges, a single transition to [final], and no tag.
   Used by [alt] to merge adjacent character classes into a single [chars]
   node instead of introducing an epsilon fork. *)
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
    let end_node =
      new_tagged_node (Set_position { cell = end_tag; offset = 0 })
    in
    end_node.eps <- [succ];
    let inner = r end_node in
    let start_node =
      new_tagged_node (Set_position { cell = start_tag; offset = 0 })
    in
    start_node.eps <- [inner];
    start_node
  in
  (wrapped, start_tag, end_tag)

let bind_start_only r =
  let start_tag = new_tag () in
  let wrapped succ =
    let inner = r succ in
    let start_node =
      new_tagged_node (Set_position { cell = start_tag; offset = 0 })
    in
    start_node.eps <- [inner];
    start_node
  in
  (wrapped, start_tag)

let bind_end_only r =
  let end_tag = new_tag () in
  let wrapped succ =
    let end_node =
      new_tagged_node (Set_position { cell = end_tag; offset = 0 })
    in
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

(* [compile_re re] instantiates a regexp by creating a fresh final node
   and passing it as the successor. Returns [(entry_node, final_node)]. *)
let compile_re re =
  let final = new_node () in
  (re final, final)

(* Determinization *)

type state = node list
(* A DFA state is a set of NFA nodes (subset construction).
   Membership is checked by physical identity (List.memq) since each
   node is created exactly once by new_node/new_tagged_node. *)

(* [add_node (state, tags) node] adds [node] to the NFA-node set [state]
   via epsilon closure: it follows all epsilon edges recursively, collecting
   any tag operations encountered along the way. Returns the updated
   (state, tags) pair. Physical identity (memq) prevents revisiting nodes. *)
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
      | Set_position _ -> ())
    tags;
  List.filter
    (function
      | Set_value (cell, value) -> Hashtbl.find dominated cell = value
      | Set_position _ -> true)
    tags

(* [transition state] computes all outgoing DFA transitions from a DFA state.
   Three phases:
   1. Normalize: collect all NFA transitions from all nodes in [state],
      sort by target node id, and merge char sets for identical targets.
   2. Split: make char sets pairwise disjoint so each DFA transition fires
      for an unambiguous set of code points.
   3. Epsilon closure: for each disjoint char set, compute the epsilon
      closure of the target NFA nodes, collecting tag operations. *)
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

(* [compute_sccs num_states raw_dfa] computes strongly connected components
   using Tarjan's algorithm. Returns [(scc_id, scc_count)] where
   [scc_id.(s)] is the SCC index for state [s]. *)
let compute_sccs num_states raw_dfa =
  let scc_id = Array.make num_states (-1) in
  let scc_count = ref 0 in
  let index = Array.make num_states (-1) in
  let lowlink = Array.make num_states 0 in
  let on_stack = Array.make num_states false in
  let stack = ref [] in
  let idx = ref 0 in
  let rec strongconnect v =
    index.(v) <- !idx;
    lowlink.(v) <- !idx;
    incr idx;
    stack := v :: !stack;
    on_stack.(v) <- true;
    let trans, _ = raw_dfa.(v) in
    Array.iter
      (fun (_, w, _) ->
        if index.(w) = -1 then (
          strongconnect w;
          lowlink.(v) <- min lowlink.(v) lowlink.(w))
        else if on_stack.(w) then lowlink.(v) <- min lowlink.(v) index.(w))
      trans;
    if lowlink.(v) = index.(v) then (
      let id = !scc_count in
      incr scc_count;
      let rec pop () =
        match !stack with
          | w :: rest ->
              stack := rest;
              on_stack.(w) <- false;
              scc_id.(w) <- id;
              if w <> v then pop ()
          | [] -> assert false
      in
      pop ())
  in
  for v = 0 to num_states - 1 do
    if index.(v) = -1 then strongconnect v
  done;
  (scc_id, !scc_count)

(* [find_delayable_tags num_states num_rules raw_dfa tag_to_rule]
   identifies Set_position tags that can be delayed from cycle bodies
   to cycle exits.

   A tag [t] is delayable at state [s] when:
   1. [s] is part of a cycle (SCC with ≥ 2 states, or a self-loop),
   2. [s] has at least one transition leaving the SCC,
   3. [s] is not reachable without [Set_position t] firing
      (every transition entering [s] carries it), and
   4. every transition leaving the SCC from a state other than [s]
      leads only to states from which [tag_to_rule.(t)] is unreachable.

   Returns [(delayed_at, scc_id)] where [delayed_at.(s)] is the list of
   delayable tags at state [s]. *)
let find_delayable_tags num_states num_rules raw_dfa tag_to_rule =
  let scc_id, scc_count = compute_sccs num_states raw_dfa in
  let scc_size = Array.make scc_count 0 in
  Array.iter (fun id -> scc_size.(id) <- scc_size.(id) + 1) scc_id;
  let has_self_loop =
    Array.init num_states (fun s ->
        let trans, _ = raw_dfa.(s) in
        Array.exists (fun (_, target, _) -> target = s) trans)
  in
  let in_cycle s = scc_size.(scc_id.(s)) >= 2 || has_self_loop.(s) in
  let has_exit s =
    let trans, _ = raw_dfa.(s) in
    Array.exists (fun (_, target, _) -> scc_id.(target) <> scc_id.(s)) trans
  in
  (* [reachable.(s).(r)] is true if some accepting state for rule [r]
     is reachable from state [s] via transitions. *)
  let reachable =
    Array.init num_states (fun s ->
        let _, finals = raw_dfa.(s) in
        Array.copy finals)
  in
  let changed = ref true in
  while !changed do
    changed := false;
    for s = 0 to num_states - 1 do
      let trans, _ = raw_dfa.(s) in
      Array.iter
        (fun (_, target, _) ->
          for r = 0 to num_rules - 1 do
            if reachable.(target).(r) && not reachable.(s).(r) then (
              reachable.(s).(r) <- true;
              changed := true)
          done)
        trans
    done
  done;
  (* For each state in a cycle that has exits, find Set_position tags
     on every entering transition, then keep only those whose owning
     rule is unreachable from non-[s] exits of the SCC. *)
  let delayed_at = Array.make num_states [] in
  for s = 0 to num_states - 1 do
    if in_cycle s && has_exit s then (
      let entering = ref [] in
      for s' = 0 to num_states - 1 do
        let trans', _ = raw_dfa.(s') in
        Array.iter
          (fun (_, target, tags) ->
            if target = s then entering := tags :: !entering)
          trans'
      done;
      let candidates =
        match !entering with
          | [] -> []
          | first :: rest ->
              List.filter
                (fun t ->
                  match t with
                    | Set_position { offset = 0; _ } ->
                        List.for_all (List.mem t) rest
                    | _ -> false)
                first
      in
      let scc = scc_id.(s) in
      delayed_at.(s) <-
        List.filter
          (fun t ->
            match t with
              | Set_position { cell; _ } ->
                  let rule = tag_to_rule.(cell) in
                  let dominated = ref true in
                  for s' = 0 to num_states - 1 do
                    if s' <> s && scc_id.(s') = scc then (
                      let trans', _ = raw_dfa.(s') in
                      Array.iter
                        (fun (_, target, _) ->
                          if scc_id.(target) <> scc && reachable.(target).(rule)
                          then dominated := false)
                        trans')
                  done;
                  !dominated
              | _ -> false)
          candidates)
  done;
  (delayed_at, scc_id)

(* [apply_tag_delay raw_dfa delayed_at scc_id] rewrites the DFA:
   - removes delayed tags from transitions entering each state, and
   - emits [Set_position] with offset 1 on transitions leaving the cycle.

   Offset 1 records the position of the previous code point, which is
   exactly the value the original [Set_position] would have had on the
   last iteration.
   This turns O(n) tag writes per loop into O(1) on exit. *)
let apply_tag_delay raw_dfa delayed_at scc_id =
  Array.mapi
    (fun s (trans, finals) ->
      let trans =
        Array.map
          (fun (cs, target, tags) ->
            let tags =
              List.filter (fun t -> not (List.mem t delayed_at.(target))) tags
            in
            let delayed_ops =
              if delayed_at.(s) <> [] && scc_id.(target) <> scc_id.(s) then
                List.filter_map
                  (fun op ->
                    match op with
                      | Set_position { cell; _ } ->
                          Some (Set_position { cell; offset = 1 })
                      | _ -> None)
                  delayed_at.(s)
              else []
            in
            (cs, target, tags @ delayed_ops))
          trans
      in
      (trans, finals))
    raw_dfa

let tag_cell = function
  | Set_position { cell; _ } -> cell
  | Set_value (cell, _) -> cell

(* [remap_cells slot_of num_slots tag_map raw_dfa init_tags] rewrites all tag cell
   references in the DFA and init_tags through [slot_of], and composes
   [tag_map] with the remapping. Returns [(num_slots, tag_map, raw_dfa, init_tags)]. *)
let remap_cells slot_of num_slots tag_map raw_dfa init_tags =
  let tag_map = Array.map (fun c -> slot_of.(c)) tag_map in
  let remap_op = function
    | Set_position { cell; offset } ->
        Set_position { cell = slot_of.(cell); offset }
    | Set_value (c, v) -> Set_value (slot_of.(c), v)
  in
  let raw_dfa =
    Array.map
      (fun (trans, finals) ->
        let trans =
          Array.map
            (fun (cs, target, tags) ->
              (cs, target, List.sort_uniq compare (List.map remap_op tags)))
            trans
        in
        (trans, finals))
      raw_dfa
  in
  let init_tags = List.sort_uniq compare (List.map remap_op init_tags) in
  (num_slots, tag_map, raw_dfa, init_tags)

(* [cell_owners num_tags rs tag_map] maps each cell to the rule that owns
   it: -1 = unused, >= 0 = single rule, -2 = multiple rules. Walks the
   NFA of each rule to discover which tags it created. *)
let cell_owners num_tags rs tag_map =
  let num_tags_raw = Array.length tag_map in
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
  cell_to_rule

(* [forward_reachable num_tags num_states raw_dfa init_tags] computes,
   for each cell [c], the set of DFA states reachable from any transition
   that writes to [c]. Two cells whose reachable sets overlap may be
   simultaneously live and must not share a slot. *)
let forward_reachable num_tags num_states raw_dfa init_tags =
  let fwd = Array.init num_tags (fun _ -> Array.make num_states false) in
  (* Seed: mark the target of each transition that writes to cell c. *)
  for s = 0 to num_states - 1 do
    let trans, _ = raw_dfa.(s) in
    Array.iter
      (fun (_, target, tags) ->
        List.iter (fun op -> fwd.(tag_cell op).(target) <- true) tags)
      trans
  done;
  List.iter (fun op -> fwd.(tag_cell op).(0) <- true) init_tags;
  (* BFS: propagate reachability along DFA transitions. *)
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
  fwd

(* [color_cells num_tags num_states cell_to_rule fwd] assigns each cell to the
   lowest-numbered slot that does not conflict with any previously
   assigned cell. Two cells conflict when they belong to the same rule,
   when either is shared across rules, or when their forward-reachable
   sets overlap.
   Returns [(slot_of, num_slots)]. *)
let color_cells num_tags num_states cell_to_rule fwd =
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
  (slot_of, !max_slot + 1)

(* [share_cells] merges non-interfering tag cells from different rules
   into shared physical slots.
   Returns [(num_tags, tag_map, raw_dfa, init_tags)] with remapped cells,
   or the inputs unchanged if no sharing is possible. *)
let share_cells num_tags num_states rs raw_dfa init_tags tag_map =
  let cell_to_rule = cell_owners num_tags rs tag_map in
  let fwd = forward_reachable num_tags num_states raw_dfa init_tags in
  let slot_of, num_slots = color_cells num_tags num_states cell_to_rule fwd in
  if num_slots < num_tags then
    remap_cells slot_of num_slots tag_map raw_dfa init_tags
  else (num_tags, tag_map, raw_dfa, init_tags)

(* [compile rs] determinizes the NFA for an array of regexp rules.
   Each rule is compiled to an NFA (entry node, final node) pair. The initial
   DFA state is the epsilon closure of all entry nodes. States are explored
   via [transition] and memoized in a hash table keyed by NFA node lists
   (physical identity). Returns a {compiled} record with the DFA, initial
   tag operations, and total number of memory cells needed. *)
let compile rs =
  let num_rules = Array.length rs in
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
  else
    let tag_to_rule = cell_owners num_tags_raw rs (Array.init num_tags_raw Fun.id) in
    let delayed_at, scc_id =
      find_delayable_tags num_states num_rules raw_dfa tag_to_rule
    in
    let raw_dfa = apply_tag_delay raw_dfa delayed_at scc_id in
    let tag_map = Array.init num_tags_raw Fun.id in
    let num_tags, tag_map, raw_dfa, init_tags =
      if num_tags_raw <= 1 || num_rules <= 1 then
        (num_tags_raw, tag_map, raw_dfa, init_tags)
      else share_cells num_tags_raw num_states rs raw_dfa init_tags tag_map
    in
    let dfa =
      Array.map
        (fun (trans, finals) -> { trans; finals })
        raw_dfa
    in
    { dfa; init_tags = dedup_tags init_tags; num_tags; tag_map }

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
            | Set_position { cell; offset = 0 } -> Printf.sprintf "t%d" cell
            | Set_position { cell; offset } ->
                Printf.sprintf "t%d-%d" cell offset
            | Set_value (c, v) -> Printf.sprintf "d%d=%d" c v
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
