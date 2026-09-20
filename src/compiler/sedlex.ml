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
      Subset construction for tagged NFAs (Laurikari, "NFAs with Tagged
      Transitions", 2000). In the terms of Trofimovich, "Tagged
      Deterministic Finite Automata with Lookahead" (2017), a TDFA(0): the
      tag operations of an epsilon closure sit on the transition entering
      the state.

      - A DFA state is an ordered list of configurations (NFA node, tag ->
        register map). One map per path, rather than one shared vector,
        keeps captures correct when a tagged node is reachable from several
        paths at once, e.g. a Star loop whose closure contains the start of
        the following capture.
      - Order is priority. The epsilon closure is a DFS along [eps] lists
        and the first path to reach a node wins: leftmost-greedy
        disambiguation among the parses of the longest match.
      - Each tag write of a transition gets a fresh register. States are
        looked up modulo register renaming; reaching an existing state
        emits the moves realigning the registers. The operations of a
        transition form a parallel move, which the code generator
        implements by let-binding the sources it overwrites.
      - Canonical cells (cell = tag id) are what the generated bindings
        read. Only the final operations of accepting states write them,
        copying the accepting path's registers just before
        [Sedlexing.mark]; working registers live above them.

   Future work (see #175): TDFA(1) and register optimization as in
   Trofimovich 2017 (sections 6 and 7), fallback registers instead of the
   cell snapshot in [Sedlexing.mark], DFA minimization.
*)

module Cset = Cset

(* NFA *)

type tag_op =
  | Set_position of { dst : int }
  | Set_value of { dst : int; value : int }
  | Copy of { dst : int; src : int }

type node = {
  id : int;  (** Unique identifier; nodes are compared by it. *)
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

(* [r, Star r] with the body duplicated. A single loop entered at the body
   would put the exit ahead of a second iteration when the first one
   consumed nothing. *)
let plus r succ = r (rep r succ)
let eps succ = succ (* eps for epsilon *)

let rec repeat r n m succ =
  assert (0 <= n && n <= m);
  match (n, m) with
    | 0, 0 -> succ
    (* Taking an iteration comes first: repetition is greedy. *)
    | 0, m -> alt (fun succ -> r (repeat r 0 (m - 1) succ)) eps succ
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
    let end_node = new_tagged_node (Set_position { dst = end_tag }) in
    end_node.eps <- [succ];
    let inner = r end_node in
    let start_node = new_tagged_node (Set_position { dst = start_tag }) in
    start_node.eps <- [inner];
    start_node
  in
  (wrapped, start_tag, end_tag)

let bind_start_only r =
  let start_tag = new_tag () in
  let wrapped succ =
    let inner = r succ in
    let start_node = new_tagged_node (Set_position { dst = start_tag }) in
    start_node.eps <- [inner];
    start_node
  in
  (wrapped, start_tag)

let bind_end_only r =
  let end_tag = new_tag () in
  let wrapped succ =
    let end_node = new_tagged_node (Set_position { dst = end_tag }) in
    end_node.eps <- [succ];
    r end_node
  in
  (wrapped, end_tag)

let new_disc_cell () = new_tag ()

let bind_disc r cell value =
  let wrapped succ =
    let disc_node = new_tagged_node (Set_value { dst = cell; value }) in
    disc_node.eps <- [succ];
    r disc_node
  in
  wrapped

(* One rule of a [match%sedlex]: the entry and final nodes of its NFA. *)
type rule = { entry : node; final : node }

(* [compile_re re] instantiates a regexp by creating a fresh final node
   and passing it as the successor. *)
let compile_re (re : regexp) : rule =
  let final = new_node () in
  { entry = re final; final }

(* The compiled automaton *)

type accept = { rule : int; final_ops : tag_op list }

type dfa_state = {
  trans : (Cset.t * int * tag_op list) array;
  accept : accept option;
}

type dfa = dfa_state array
type compiled = { dfa : dfa; init_tags : tag_op list; num_tags : int }

let op_dest = function
  | Copy { dst; _ } | Set_position { dst } | Set_value { dst; _ } -> dst

(* Determinization (tagged subset construction, see the overview above) *)

(* A memory cell, by index. *)
type cell = int

module TagMap = Map.Make (Int)
module CellMap = Map.Make (Int)

(* What a transition writes: the current position or a discriminator. *)
type write = Position | Value of int

let op_of_write (dst : cell) : write -> tag_op = function
  | Position -> Set_position { dst }
  | Value value -> Set_value { dst; value }

(* Where a tag's value lives while a transition is computed: in a cell, or
   in a write of that transition. Same tag and same write: same register. *)
type addr = Cell of cell | Pending of write

(* One NFA path: the node it reached and where each tag it wrote lives. *)
type 'a config = { node : node; tags : 'a TagMap.t }

(* A DFA state: configurations in priority order. A [candidate] comes out
   of [eps_closure]; a [stored] state has every tag in a cell. *)
type 'a state = 'a config list
type candidate = addr state
type stored = cell state

(* Only nodes with character transitions and final nodes belong in a
   state; epsilon-only nodes would just bloat its key. *)
let is_relevant (node : node) : bool = node.trans <> [] || node.eps = []

(* DFS along [eps] lists; the first path to reach a node wins, which is the
   leftmost-greedy policy. Tag writes on the way become [Pending]. *)
let eps_closure (seeds : addr config list) : candidate =
  let visited = Hashtbl.create 16 in
  let acc = ref [] in
  let rec visit node tags =
    if not (Hashtbl.mem visited node.id) then (
      Hashtbl.add visited node.id ();
      let tags =
        match node.tag with
          | None -> tags
          | Some (Set_position { dst }) ->
              TagMap.add dst (Pending Position) tags
          | Some (Set_value { dst; value }) ->
              TagMap.add dst (Pending (Value value)) tags
          | Some (Copy _) -> assert false (* never carried by NFA nodes *)
      in
      if is_relevant node then acc := { node; tags } :: !acc;
      List.iter (fun n -> visit n tags) node.eps)
  in
  List.iter (fun c -> visit c.node c.tags) seeds;
  List.rev !acc

(* Splits the moves into pairwise-disjoint character sets. Moves come in
   priority order and a configuration is appended last to each piece it
   overlaps, so seeds stay in priority order. *)
let split_moves (moves : (Cset.t * addr config) list) :
    (Cset.t * addr config list) list =
  let add_move pieces (cset, cfg) =
    let rec insert cset pieces =
      if Cset.is_empty cset then pieces
      else (
        match pieces with
          | [] -> [(cset, [cfg])]
          | (piece, seeds) :: rest ->
              let inter = Cset.intersection piece cset in
              if Cset.is_empty inter then (piece, seeds) :: insert cset rest
              else (
                let piece_only = Cset.difference piece inter in
                let cset_rest = Cset.difference cset inter in
                let with_cfg = (inter, seeds @ [cfg]) in
                if Cset.is_empty piece_only then
                  with_cfg :: insert cset_rest rest
                else (piece_only, seeds) :: with_cfg :: insert cset_rest rest))
    in
    insert cset pieces
  in
  List.fold_left add_move [] moves

(* Mutable state of the construction *)

(* Cell allocation. Canonical cells [0 .. num_logical - 1] (cell = tag id)
   are only written by final operations; working registers live above. *)
module Registers : sig
  type t

  val create : num_logical:int -> t

  (* A fresh working register. *)
  val alloc : t -> cell

  (* As opposed to a canonical cell. *)
  val is_working : t -> cell -> bool

  (* Canonical and working cells. *)
  val count : t -> int
end = struct
  type t = { num_logical : int; mutable next_cell : int }

  let create ~num_logical = { num_logical; next_cell = num_logical }

  let alloc t =
    let c = t.next_cell in
    t.next_cell <- c + 1;
    c

  let is_working t c = c >= t.num_logical
  let count t = t.next_cell
end

(* State identity modulo register renaming: registers are numbered by first
   occurrence, so equal keys mean same nodes, same order, same sharing. *)
module State_key : sig
  type t

  val of_candidate : candidate -> t

  module Tbl : Hashtbl.S with type key = t
end = struct
  type t = (int * (int * int) list) list

  let of_candidate configs =
    let tbl = Hashtbl.create 8 in
    let canon tag a =
      match Hashtbl.find_opt tbl (tag, a) with
        | Some i -> i
        | None ->
            let i = Hashtbl.length tbl in
            Hashtbl.add tbl (tag, a) i;
            i
    in
    List.map
      (fun c ->
        ( c.node.id,
          List.map (fun (t, a) -> (t, canon t a)) (TagMap.bindings c.tags) ))
      configs

  module Tbl = Hashtbl.Make (struct
    type nonrec t = t

    let equal = ( = )
    let hash = Hashtbl.hash
  end)
end

(* DFA states discovered so far, numbered in creation order. *)
type state_table = {
  by_key : int State_key.Tbl.t;
  configs : (int, stored) Hashtbl.t;
  defs : (int, dfa_state) Hashtbl.t;
  mutable n_states : int;
}

let make_state_table () : state_table =
  {
    by_key = State_key.Tbl.create 31;
    configs = Hashtbl.create 31;
    defs = Hashtbl.create 31;
    n_states = 0;
  }

(* Registers a new state and returns its number. *)
let add_state (tbl : state_table) (key : State_key.t) (configs : stored) : int =
  let num = tbl.n_states in
  tbl.n_states <- num + 1;
  State_key.Tbl.add tbl.by_key key num;
  Hashtbl.add tbl.configs num configs;
  num

type ctx = { regs : Registers.t; rules : rule array; tbl : state_table }

(* New state: each [Pending] write gets a fresh cell. Returns the stored
   state and the Set operations of the transition reaching it. *)
let assign_cells (regs : Registers.t) (candidate : candidate) :
    stored * tag_op list =
  let assigned = Hashtbl.create 4 in
  let ops = ref [] in
  let cell_for_new tag w =
    match Hashtbl.find_opt assigned (tag, w) with
      | Some c -> c
      | None ->
          let c = Registers.alloc regs in
          Hashtbl.add assigned (tag, w) c;
          ops := op_of_write c w :: !ops;
          c
  in
  let configs =
    List.map
      (fun c ->
        {
          c with
          tags =
            TagMap.mapi
              (fun tag a ->
                match a with
                  | Cell cell -> cell
                  | Pending w -> cell_for_new tag w)
              c.tags;
        })
      candidate
  in
  (configs, !ops)

(* Moves realigning [candidate]'s registers with those of the equal-keyed
   [existing] state. A parallel move. *)
let register_moves (candidate : candidate) (existing : stored) : tag_op list =
  let moves = ref CellMap.empty in
  let set_move dst op =
    match CellMap.find_opt dst !moves with
      | None -> moves := CellMap.add dst op !moves
      | Some op' -> assert (op = op')
  in
  List.iter2
    (fun cand ex ->
      TagMap.iter
        (fun tag a ->
          let dst = TagMap.find tag ex.tags in
          match a with
            | Cell src -> if src <> dst then set_move dst (Copy { dst; src })
            | Pending w -> set_move dst (op_of_write dst w))
        cand.tags)
    candidate existing;
  List.map snd (CellMap.bindings !moves)

(* Copies the accepting configuration's registers into the canonical cells.
   Working sources, canonical destinations: the copies cannot interfere. *)
let final_ops_of (regs : Registers.t) (accepting : cell config) : tag_op list =
  TagMap.fold
    (fun tag cell acc ->
      if cell = tag then acc
      else (
        assert (Registers.is_working regs cell);
        Copy { dst = tag; src = cell } :: acc))
    accepting.tags []

(* The accepting rule is the lowest-numbered one whose final node is in the
   state (first-match semantics), with its final operations. *)
let accept_of (ctx : ctx) (configs : stored) : accept option =
  let n = Array.length ctx.rules in
  let rec first_accepting rule =
    if rule = n then None
    else (
      let final = ctx.rules.(rule).final in
      match List.find_opt (fun c -> c.node == final) configs with
        | Some accepting ->
            Some { rule; final_ops = final_ops_of ctx.regs accepting }
        | None -> first_accepting (rule + 1))
  in
  first_accepting 0

(* The number of [candidate]'s state, built depth first if new, and the
   operations of the transition reaching it. *)
let rec find_or_add_state (ctx : ctx) (candidate : candidate) :
    int * tag_op list =
  let key = State_key.of_candidate candidate in
  match State_key.Tbl.find_opt ctx.tbl.by_key key with
    | Some num ->
        (num, register_moves candidate (Hashtbl.find ctx.tbl.configs num))
    | None ->
        let configs, ops = assign_cells ctx.regs candidate in
        let num = add_state ctx.tbl key configs in
        build_state ctx num configs;
        (num, ops)

(* Computes and stores the definition of state [num]. *)
and build_state (ctx : ctx) (num : int) (configs : stored) : unit =
  let trans = transitions ctx configs in
  let accept = accept_of ctx configs in
  Hashtbl.add ctx.tbl.defs num { trans; accept }

(* Outgoing transitions: split the character moves into disjoint sets,
   close each piece, and look the pieces up in character-set order, so that
   state numbers follow that order. *)
and transitions (ctx : ctx) (configs : stored) :
    (Cset.t * int * tag_op list) array =
  let moves =
    List.concat
      (List.map
         (fun c ->
           let tags = TagMap.map (fun cell -> Cell cell) c.tags in
           List.map (fun (cset, node) -> (cset, { node; tags })) c.node.trans)
         configs)
  in
  let pieces =
    Array.of_list
      (List.map
         (fun (cset, seeds) -> (cset, eps_closure seeds))
         (split_moves moves))
  in
  Array.sort (fun (c1, _) (c2, _) -> compare c1 c2) pieces;
  Array.map
    (fun (cset, candidate) ->
      let num, ops = find_or_add_state ctx candidate in
      (cset, num, ops))
    pieces

(* See the implementation overview at the top of this file. *)
let compile (rs : regexp array) : compiled =
  let rules = Array.map compile_re rs in
  let ctx =
    {
      regs = Registers.create ~num_logical:!cur_tag;
      rules;
      tbl = make_state_table ();
    }
  in
  let seeds =
    List.map
      (fun r -> { node = r.entry; tags = TagMap.empty })
      (Array.to_list rules)
  in
  let num0, init_tags = find_or_add_state ctx (eps_closure seeds) in
  assert (num0 = 0);
  {
    dfa = Array.init ctx.tbl.n_states (Hashtbl.find ctx.tbl.defs);
    init_tags;
    num_tags = Registers.count ctx.regs;
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
        add_discriminators lowered
    | Ir.Seq elems ->
        (* Sequence — propagate left/right position contexts through elements.
           Right positions are computed right-to-left; left positions are
           updated left-to-right after lowering each element. *)
        let _, rights =
          List.fold_right
            (fun e (acc, l) -> (retreat acc (Ir.fixed_length e), acc :: l))
            elems (right, [])
        in
        (* Fallback for the left context: if [advance] returns [None]
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
        (* [seq] is function composition and [eps] its identity, so the
           fold needs no special case for the first element. *)
        let _, r_acc, tags_acc =
          List.fold_left2
            (fun (cur_left, r_acc, tags_acc) e right ->
              let r', tags' = lower ~left:cur_left ~right e in
              let new_left =
                match advance cur_left (Ir.fixed_length e) with
                  | Some _ as s -> s
                  | None -> left_from_end_tag e tags'
              in
              (new_left, seq r_acc r', tags_acc @ tags'))
            (left, eps, []) elems rights
        in
        (r_acc, tags_acc)

let compile_ir (rules : Ir.t array) =
  Array.iter (fun ir -> Ir.check_invariant ir) rules;
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

let tag_op_to_string = function
  | Set_position { dst } -> "t" ^ string_of_int dst
  | Set_value { dst; value } ->
      "d" ^ string_of_int dst ^ "=" ^ string_of_int value
  | Copy { dst; src } -> "t" ^ string_of_int dst ^ "<-t" ^ string_of_int src

let dfa_to_dot dfa =
  let buf = Buffer.create 1024 in
  let bprintf = Printf.bprintf in
  bprintf buf "digraph {\n";
  bprintf buf "  rankdir=LR;\n";
  bprintf buf "  node [shape=circle];\n\n";
  bprintf buf "  _start [shape=point];\n";
  bprintf buf "  _start -> state0;\n\n";
  Array.iteri
    (fun i { trans; accept } ->
      (match accept with
        | None -> bprintf buf "  state%d [label=\"%d\"];\n" i i
        | Some { rule; final_ops } ->
            let ops =
              if final_ops = [] then ""
              else
                "\\n{"
                ^ String.concat "," (List.map tag_op_to_string final_ops)
                ^ "}"
            in
            bprintf buf
              "  state%d [label=\"%d\\n[rule %d]%s\", shape=doublecircle];\n" i
              i rule ops);
      Array.iter
        (fun (cset, target, tags) ->
          let label = cset_to_label cset in
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
