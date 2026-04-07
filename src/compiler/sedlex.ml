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
      [bind] wraps a sub-regexp with start/end tagged epsilon nodes so the
      DFA can record sub-match positions at runtime. When the PPX can
      compute one boundary from a known offset (see [pos_expr] in
      ppx_sedlex.ml), [bind_start_only] or [bind_end_only] is used instead,
      saving a memory cell. Discriminator tags (Set_value) disambiguate
      or-patterns where multiple branches bind the same name.

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
   - Self-loop tag delay: tags on self-loops that also appear on all
     entering transitions can be removed from those transitions and emitted
     as a "set previous position" on exit. This turns O(n) tag writes in
     loops (e.g. Star) into O(1) on exit.
   - Intra-rule tag coalescing: tags with identical occurrence signatures
     (same presence in init_tags and same set of transitions) can share a
     single memory cell.
   - Cross-rule cell sharing: memory cells from non-interfering rules can
     share the same physical slot via liveness analysis and graph coloring.

   DFA construction:
   - DFA minimization: the generated DFA is not minimized. Hopcroft's or
     Moore's algorithm could reduce state count, especially for patterns with
     many character classes that converge to the same accepting state.
*)

module Cset = Cset

(* NFA *)

type tag_op = Set_position of int | Set_value of int * int

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

let rec repeat r n m succ =
  assert (0 <= n && n <= m);
  match (n, m) with
    | 0, 0 -> succ
    | 0, m -> alt eps (fun succ -> r (repeat r 0 (m - 1) succ)) succ
    | n, m -> r (repeat r (n - 1) (m - 1) succ)

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
type compiled = { dfa : dfa; init_tags : tag_op list; num_tags : int }

(* [compile rs] determinizes the NFA for an array of regexp rules.
   Each rule is compiled to an NFA (entry node, final node) pair. The initial
   DFA state is the epsilon closure of all entry nodes. States are explored
   via [transition] and memoized in a hash table keyed by NFA node lists
   (physical identity). Returns a {compiled} record with the DFA, initial
   tag operations, and total number of memory cells needed. *)
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
      let trans = Array.map (fun (p, t, tags) -> (p, aux t, tags)) trans in
      let finals = Array.map (fun (_, f) -> List.memq f state) rs in
      Hashtbl.add states_def i { trans; finals };
      i
  in
  let init = ref ([], []) in
  Array.iter (fun (i, _) -> init := add_node !init i) rs;
  let init_state, init_tags = !init in
  let i = aux init_state in
  assert (i = 0);
  {
    dfa = Array.init !counter (Hashtbl.find states_def);
    init_tags = dedup_tags init_tags;
    num_tags = !cur_tag;
  }

(* High-level compilation from IR.

   [compile_ir] lowers [Ir.t] patterns into low-level regexps with tag
   annotations, then compiles them via [compile]. The lowering phase decides
   how to allocate tags for [as] bindings:
   - [Start_plus n]: the position is [n] code points from the token start.
   - [End_minus n]: the position is [n] code points before the token end.
   - [Tag {tag; offset}]: read memory cell [tag] and add [offset].
   When both boundaries of a capture can be expressed as [Start_plus] or
   [End_minus], no memory cells are needed at all.

   Or-patterns [(p1 as x) | (p2 as x)] additionally use discriminator cells:
   integer values that record which branch was taken, so the code generator
   can emit the correct position extraction at match time. *)

type pos_expr =
  | Tag of { tag : int; offset : int }
  | Start_plus of int
  | End_minus of int

type compiled_binding = {
  name : string;
  start_pos : pos_expr;
  end_pos : pos_expr;
  disc : (int * int) list;
}

type compiled_ir = {
  dfa : dfa;
  init_tags : tag_op list;
  num_tags : int;
  bindings : compiled_binding list array;
}

(* [shift_pos pe delta] shifts a position expression by [delta] code points
   (positive = forward, negative = backward). Returns [None] if either
   argument is unknown. *)
let shift_pos pe delta =
  match (pe, delta) with
    | Some (Start_plus n), Some d -> Some (Start_plus (n + d))
    | Some (End_minus n), Some d -> Some (End_minus (n - d))
    | Some (Tag { tag; offset }), Some d ->
        Some (Tag { tag; offset = offset + d })
    | _ -> None

let advance pe len = shift_pos pe len
let retreat pe len = shift_pos pe (Option.map Int.neg len)

(* [add_discriminators branches] takes a list of [(regexp, bindings)] pairs
   from an n-ary alternation and wraps each branch with a discriminator tag
   so the generated code can tell which branch matched. Branches with
   identical bindings share the same discriminator value. If all branches
   have identical bindings, no discriminator cell is allocated. *)
let add_discriminators (branches : (regexp * compiled_binding list) list) =
  let fold_alt = function
    | [] -> assert false
    | (r, _) :: rest -> List.fold_left (fun acc (r, _) -> alt acc r) r rest
  in
  (* Check if all branches produce identical bindings — if so, no
     discriminator is needed at all. *)
  let all_same =
    match branches with
      | [] | [_] -> true
      | (_, first) :: rest -> List.for_all (fun (_, tags) -> tags = first) rest
  in
  if all_same then (fold_alt branches, snd (List.hd branches))
  else (
    let disc_cell = new_disc_cell () in
    let stamp value tags =
      List.map
        (fun (ti : compiled_binding) ->
          { ti with disc = (disc_cell, value) :: ti.disc })
        tags
    in
    (* Assign discriminator values. Branches with identical bindings
       share the same value. *)
    let next_val = ref 0 in
    let seen : (compiled_binding list * int) list ref = ref [] in
    let get_value tags =
      match List.assoc_opt tags !seen with
        | Some v -> v
        | None ->
            let v = !next_val in
            incr next_val;
            seen := (tags, v) :: !seen;
            v
    in
    let wrapped =
      List.map
        (fun (r, tags) ->
          let v = get_value tags in
          (bind_disc r disc_cell v, stamp v tags))
        branches
    in
    (fold_alt wrapped, List.concat_map snd wrapped))

(* [lower ir ~left ~right] converts an IR pattern to a low-level regexp
   and a list of compiled bindings. [left] and [right] are the known
   position contexts at the start and end of this pattern element. *)
let rec lower ~left ~right (ir : Ir.t) : regexp * compiled_binding list =
  match ir with
    | Ir.Chars cset -> (chars cset, [])
    | Ir.Eps -> (eps, [])
    | Ir.Star inner ->
        let r, _ = lower ~left:None ~right:None inner in
        (rep r, [])
    | Ir.Plus inner ->
        let r, _ = lower ~left:None ~right:None inner in
        (plus r, [])
    | Ir.Rep (inner, n, m) ->
        let r, _ = lower ~left:None ~right:None inner in
        (repeat r n m, [])
    | Ir.Capture (name, inner) ->
        (* Named capture — try to derive each boundary from [left]/[right]
           context or [fixed_length]; allocate tags only for boundaries that
           cannot be computed statically. Best case: 0 tags. Worst case: 2. *)
        let r, tags = lower ~left ~right inner in
        let elem_len = Ir.fixed_length inner in
        let known_start =
          match left with Some _ -> left | None -> retreat right elem_len
        in
        let known_end =
          match right with
            | Some _ -> right
            | None -> advance known_start elem_len
        in
        let st, et, r =
          match (known_start, known_end) with
            | Some st, Some et -> (st, et, r)
            | Some st, None ->
                let wrapped, end_tag = bind_end_only r in
                (st, Tag { tag = end_tag; offset = 0 }, wrapped)
            | None, Some et ->
                let wrapped, start_tag = bind_start_only r in
                (Tag { tag = start_tag; offset = 0 }, et, wrapped)
            | None, None -> (
                match elem_len with
                  | Some len ->
                      let wrapped, start_tag = bind_start_only r in
                      ( Tag { tag = start_tag; offset = 0 },
                        Tag { tag = start_tag; offset = len },
                        wrapped )
                  | None ->
                      let wrapped, start_tag, end_tag = bind r in
                      ( Tag { tag = start_tag; offset = 0 },
                        Tag { tag = end_tag; offset = 0 },
                        wrapped ))
        in
        (r, { name; start_pos = st; end_pos = et; disc = [] } :: tags)
    | Ir.Alt branches ->
        let lowered = List.map (lower ~left ~right) branches in
        let has_captures = List.exists (fun (_, tags) -> tags <> []) lowered in
        if has_captures then add_discriminators lowered
        else (
          let r =
            List.fold_left
              (fun acc (r, _) -> alt acc r)
              (fst (List.hd lowered))
              (List.tl lowered)
          in
          (r, []))
    | Ir.Seq elems ->
        (* Sequence — propagate left/right position contexts through elements.
           Right positions are computed right-to-left; left positions are
           updated left-to-right after lowering each element. *)
        let n = List.length elems in
        let lengths = List.map Ir.fixed_length elems in
        let lengths_arr = Array.of_list lengths in
        (* Compute right positions (right-to-left) *)
        let rights = Array.make n None in
        let () =
          let acc = ref right in
          for i = n - 1 downto 0 do
            rights.(i) <- !acc;
            acc := retreat !acc lengths_arr.(i)
          done
        in
        (* Fallback for [update_left]: if [advance] returns [None]
           (because the current left is unknown or the element has
           variable length), but the element was a [Capture] whose
           end position is a [Tag], we can use that tag as the [left]
           anchor for the next element — it records a runtime position.
           [Start_plus]/[End_minus] endpoints don't help here: they
           are already factored into [advance], so if [advance] failed,
           they have nothing more to offer. *)
        let left_from_end_tag ir tags' =
          match ir with
            | Ir.Capture _ -> (
                match tags' with
                  | { end_pos = Tag _ as et; _ } :: _ -> Some et
                  | _ -> None)
            | _ -> None
        in
        let update_left cur i ir tags' =
          match advance cur lengths_arr.(i) with
            | Some _ as s -> s
            | None -> left_from_end_tag ir tags'
        in
        let elems_arr = Array.of_list elems in
        let r0, tags0 = lower ~left ~right:rights.(0) elems_arr.(0) in
        let left0 = update_left left 0 elems_arr.(0) tags0 in
        let _, _, r_acc, tags_acc =
          Array.fold_left
            (fun (i, cur_left, r_acc, tags_acc) ir_elem ->
              if i = 0 then (1, left0, r_acc, tags_acc)
              else (
                let r', tags' =
                  lower ~left:cur_left ~right:rights.(i) ir_elem
                in
                let new_left = update_left cur_left i ir_elem tags' in
                (i + 1, new_left, seq r_acc r', tags_acc @ tags')))
            (0, left, r0, tags0) elems_arr
        in
        (r_acc, tags_acc)

let compile_ir (rules : Ir.t array) =
  Array.iter
    (fun ir ->
      match Ir.validate ir with Ok () -> () | Error msg -> invalid_arg msg)
    rules;
  reset_tags ();
  let lowered =
    Array.map
      (fun ir ->
        lower ~left:(Some (Start_plus 0)) ~right:(Some (End_minus 0)) ir)
      rules
  in
  let regexps = Array.map fst lowered in
  let bindings = Array.map snd lowered in
  let compiled = compile regexps in
  {
    dfa = compiled.dfa;
    init_tags = compiled.init_tags;
    num_tags = compiled.num_tags;
    bindings;
  }

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
