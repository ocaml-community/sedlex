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
      Subset construction extended to tagged NFAs (Laurikari, "NFAs with
      Tagged Transitions", 2000), following the structure of ocamllex's
      implementation (lex/lexgen.ml in the OCaml distribution).

      A DFA state is an ordered list of configurations: (NFA node, register
      map). The register map records, per logical tag, which memory cell
      holds that tag's position *along the NFA path that reached this node*.
      Keeping one map per configuration, instead of one shared vector, is
      what makes captures correct when a tagged epsilon node is reachable
      from several paths at once (e.g. a Star loop whose epsilon closure
      contains the start node of a following capture: the loop path re-fires
      the tag write on every iteration, while the path already inside the
      capture must keep the earlier position).

      Configuration order is priority: epsilon closure visits nodes
      depth-first following the order of [eps] lists (alternation prefers
      the left branch, repetition prefers continuing the loop), and the
      first path to reach a node wins. This yields leftmost-greedy
      disambiguation of capture positions among parses of the (always
      longest) match.

      Tag writes performed by a transition allocate fresh registers; the
      target DFA state is looked up modulo a bijective renaming of
      registers (the canonical key numbers registers by first occurrence).
      When the lookup hits an existing state, register-move operations
      (Copy/Set) are emitted on the transition to realign registers with
      the existing state's maps. The operations of one transition form a
      parallel move: every Copy reads its source as it was before the
      transition's writes. The code generator implements this by saving
      clobbered sources in let-bound locals, so no move ordering or
      temporary cells are needed (ocamllex needs both because its moves
      are interpreted by a fixed C engine with no scratch locals).

      Accepting states carry final operations: Copy operations that
      materialize the accepting configuration's registers into the
      canonical cells (cell index = logical tag id) read by the generated
      bindings. They run just before [Sedlexing.mark], so the
      snapshot/backtrack machinery needs no changes.

   Possible future optimizations (see #175)
   -----------------------------------------

   - Self-loop tag delay: tags rewritten on every iteration of a self-loop
     could be maintained as a "previous position" delta and written once on
     exit, turning O(n) writes into O(1).
   - DFA minimization: the generated DFA is not minimized. Hopcroft's or
     Moore's algorithm could reduce state count, especially for patterns with
     many character classes that converge to the same accepting state.
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

(* Determinization (tagged subset construction, see the overview above) *)

module TagMap = Map.Make (Int)

(* What a transition writes into a register: the current position, or a
   discriminator value. *)
type write = Wpos | Wval of int

let op_of_write (dst : int) : write -> tag_op = function
  | Wpos -> Set_position { dst }
  | Wval value -> Set_value { dst; value }

(* During transition computation, a logical tag maps to either a concrete
   memory cell ([Old]) or a register written by the pending transition
   ([New]). Within one transition, two [New] addresses of the same tag
   denote the same register exactly when they hold the same write. *)
type addr = Old of int | New of write

(* One active NFA path: the node it reached and, for each logical tag
   written along the path, the register holding the recorded value. A DFA
   state is a [config list] in priority order; in stored states all
   addresses are [Old]. *)
type config = { node : node; tags : addr TagMap.t }

(* [closure seeds] computes the priority-ordered epsilon closure of
   [seeds]. Nodes are visited depth-first following the order of [eps]
   lists, and the first (highest-priority) path to reach a node fixes that
   node's register map: this implements leftmost-greedy disambiguation.
   Tag writes encountered along the way become [New] addresses. *)
let closure (seeds : config list) : config list =
  let visited = Hashtbl.create 16 in
  let acc = ref [] in
  let rec visit node tags =
    if not (Hashtbl.mem visited node.id) then (
      Hashtbl.add visited node.id ();
      let tags =
        match node.tag with
          | None -> tags
          | Some (Set_position { dst }) -> TagMap.add dst (New Wpos) tags
          | Some (Set_value { dst; value }) ->
              TagMap.add dst (New (Wval value)) tags
          | Some (Copy _) -> assert false (* never carried by NFA nodes *)
      in
      acc := { node; tags } :: !acc;
      List.iter (fun n -> visit n tags) node.eps)
  in
  List.iter (fun c -> visit c.node c.tags) seeds;
  List.rev !acc

(* [split_moves moves] partitions the character transitions leaving a DFA
   state into pairwise-disjoint character sets. Each resulting piece
   carries its seed configurations in the original (priority) order. *)
let split_moves (moves : (Cset.t * config) list) : (Cset.t * config list) list =
  let add pieces (c, cfg) =
    let rec ins c pieces =
      if Cset.is_empty c then pieces
      else (
        match pieces with
          | [] -> [(c, [cfg])]
          | (pc, seeds) :: rest ->
              let inter = Cset.intersection pc c in
              if Cset.is_empty inter then (pc, seeds) :: ins c rest
              else (
                let pc_only = Cset.difference pc inter in
                let c_rest = Cset.difference c inter in
                let with_cfg = (inter, seeds @ [cfg]) in
                if Cset.is_empty pc_only then with_cfg :: ins c_rest rest
                else (pc_only, seeds) :: with_cfg :: ins c_rest rest))
    in
    ins c pieces
  in
  List.fold_left add [] moves

type accept = { rule : int; final_ops : tag_op list }

type dfa_state = {
  trans : (Cset.t * int * tag_op list) array;
  accept : accept option;
}

type dfa = dfa_state array
type compiled = { dfa : dfa; init_tags : tag_op list; num_tags : int }

let op_dest = function
  | Copy { dst; _ } | Set_position { dst } | Set_value { dst; _ } -> dst

(* [lowest_final rules is_final] is the lowest-numbered rule whose final
   node satisfies [is_final], i.e. the highest-priority accepting rule of a
   state under the first-match semantics of [match%sedlex]. *)
let lowest_final (rules : rule array) (is_final : node -> bool) : int option =
  let n = Array.length rules in
  let rec aux i =
    if i = n then None
    else if is_final rules.(i).final then Some i
    else aux (i + 1)
  in
  aux 0

(* === Determinization state ===

   The subset construction threads mutable state, made explicit as
   records: [registers] (memory-cell allocation) and [state_table] (the DFA
   states discovered so far), grouped with the compiled rules in [ctx]. *)

(* Memory-cell allocation state. *)
type registers = {
  num_logical : int;
      (* Number of logical tags. Canonical cells 0..num_logical-1 are
         written only by final operations and read by the generated
         binding-extraction code; working registers live above them. *)
  mutable next_cell : int; (* Next fresh working register. *)
  pools : (int, int list) Hashtbl.t;
      (* Per-logical-tag pool of working registers allocated so far;
         reusing them keeps the total cell count small. Pools of distinct
         tags are disjoint. *)
}

let make_registers (num_logical : int) : registers =
  { num_logical; next_cell = num_logical; pools = Hashtbl.create 8 }

(* [alloc_cell regs used tag] picks a working register for [tag],
   preferring a register from the tag's pool not already in [used], and
   adds it to [used]. *)
let alloc_cell (regs : registers) (used : int list ref) (tag : int) : int =
  let pool =
    match Hashtbl.find_opt regs.pools tag with Some l -> l | None -> []
  in
  let c =
    match List.find_opt (fun c -> not (List.mem c !used)) pool with
      | Some c -> c
      | None ->
          let c = regs.next_cell in
          regs.next_cell <- c + 1;
          Hashtbl.replace regs.pools tag (c :: pool);
          c
  in
  used := c :: !used;
  c

(* The identity of a DFA state. States are looked up modulo bijective
   register renaming: the key numbers each distinct register (a tag's
   address) by first occurrence, so two configuration lists get the same
   key exactly when they have the same nodes in the same order and the same
   register-sharing structure. *)
module State_key : sig
  type t

  val of_configs : config list -> t

  module Tbl : Hashtbl.S with type key = t
end = struct
  type t = (int * (int * int) list) list

  let of_configs configs =
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
  configs : (int, config list) Hashtbl.t;
      (* Stored configurations; all addresses are [Old]. *)
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

(* [add_state tbl key configs] registers a new state and returns its
   number. *)
let add_state (tbl : state_table) (key : State_key.t) (configs : config list) :
    int =
  let num = tbl.n_states in
  tbl.n_states <- num + 1;
  State_key.Tbl.add tbl.by_key key num;
  Hashtbl.add tbl.configs num configs;
  num

type ctx = { regs : registers; rules : rule array; tbl : state_table }

(* [old_cells configs] lists the concrete cells [configs] already hold,
   which a new register must not reuse. *)
let old_cells (configs : config list) : int list =
  List.concat
    (List.map
       (fun c ->
         List.filter_map
           (fun (_, a) -> match a with Old c -> Some c | New _ -> None)
           (TagMap.bindings c.tags))
       configs)

(* Creating a new state: [Old] registers are kept as-is, [New] writes get
   concrete cells; the transition only carries the Set operations. *)
let concretize (regs : registers) (configs : config list) :
    config list * tag_op list =
  let used = ref (old_cells configs) in
  let assigned = Hashtbl.create 4 in
  let ops = ref [] in
  let cell_for_new tag w =
    match Hashtbl.find_opt assigned (tag, w) with
      | Some c -> c
      | None ->
          let c = alloc_cell regs used tag in
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
                match a with Old _ -> a | New w -> Old (cell_for_new tag w))
              c.tags;
        })
      configs
  in
  (configs, !ops)

(* Reaching an existing state: emit the register moves that realign the
   candidate's registers with the stored state's maps. Equal canonical
   keys guarantee each destination cell gets a single consistent move.
   The result is a parallel move (Copy sources observe the pre-transition
   state); it is sorted by destination only for output stability. *)
let moves_to (candidate : config list) (existing : config list) : tag_op list =
  let moves = Hashtbl.create 8 in
  let set_move dst op =
    match Hashtbl.find_opt moves dst with
      | None -> Hashtbl.add moves dst op
      | Some op' -> assert (op = op')
  in
  List.iter2
    (fun cand ex ->
      TagMap.iter
        (fun tag a ->
          let dst =
            match TagMap.find tag ex.tags with
              | Old c -> c
              | New _ -> assert false
          in
          match a with
            | Old src -> if src <> dst then set_move dst (Copy { dst; src })
            | New w -> set_move dst (op_of_write dst w))
        cand.tags)
    candidate existing;
  let mvs = Hashtbl.fold (fun _ op acc -> op :: acc) moves [] in
  List.sort (fun a b -> compare (op_dest a) (op_dest b)) mvs

(* [final_ops_of ctx configs i] materializes accepting rule [i]'s
   configuration registers into the canonical cells (cell = logical tag
   id), to run just before [Sedlexing.mark]. Sources are working registers
   (>= num_logical) and destinations canonical cells, so the copies never
   interfere with each other. *)
let final_ops_of (ctx : ctx) (configs : config list) (i : int) : tag_op list =
  let fin = ctx.rules.(i).final in
  let accepting = List.find (fun c -> c.node == fin) configs in
  TagMap.fold
    (fun tag a acc ->
      match a with
        | Old c ->
            if c = tag then acc
            else (
              assert (c >= ctx.regs.num_logical);
              Copy { dst = tag; src = c } :: acc)
        | New _ -> assert false)
    accepting.tags []

(* [accept_of ctx configs] resolves rule priority: the accepting state
   carries the lowest-numbered (highest-priority) rule whose final node is
   in [configs] and the operations materializing its registers. *)
let accept_of (ctx : ctx) (configs : config list) : accept option =
  let is_final fin = List.exists (fun c -> c.node == fin) configs in
  match lowest_final ctx.rules is_final with
    | None -> None
    | Some rule -> Some { rule; final_ops = final_ops_of ctx configs rule }

(* [get_state ctx candidate] returns the state number for
   [candidate], creating it (and, depth first, everything reachable from
   it) if new, plus the tag operations the transition reaching it must
   perform. *)
let rec get_state (ctx : ctx) (candidate : config list) : int * tag_op list =
  let key = State_key.of_configs candidate in
  match State_key.Tbl.find_opt ctx.tbl.by_key key with
    | Some num -> (num, moves_to candidate (Hashtbl.find ctx.tbl.configs num))
    | None ->
        let configs, ops = concretize ctx.regs candidate in
        let num = add_state ctx.tbl key configs in
        build_state ctx num configs;
        (num, ops)

(* [build_state ctx num configs] computes and stores the definition of
   state [num]. *)
and build_state (ctx : ctx) (num : int) (configs : config list) : unit =
  let trans = transition ctx configs in
  let accept = accept_of ctx configs in
  Hashtbl.add ctx.tbl.defs num { trans; accept }

(* [transition ctx configs] builds the outgoing transitions of one DFA
   state: collect the character moves of every configuration, split them
   into pairwise-disjoint sets, close each piece over epsilon edges, then
   look the pieces up in character-set order, so that state numbers follow
   that order depth first. *)
and transition (ctx : ctx) (configs : config list) :
    (Cset.t * int * tag_op list) array =
  let moves =
    List.concat
      (List.map
         (fun c ->
           List.map (fun (cset, n) -> (cset, { c with node = n })) c.node.trans)
         configs)
  in
  let pieces =
    Array.of_list
      (List.map
         (fun (cset, seeds) -> (cset, closure seeds))
         (split_moves moves))
  in
  Array.sort (fun (c1, _) (c2, _) -> compare c1 c2) pieces;
  Array.map
    (fun (cset, candidate) ->
      let num, ops = get_state ctx candidate in
      (cset, num, ops))
    pieces

(* [compile rs] determinizes the NFA for an array of regexp rules. See the
   implementation overview at the top of this file. *)
let compile (rs : regexp array) : compiled =
  let rules = Array.map compile_re rs in
  let ctx =
    { regs = make_registers !cur_tag; rules; tbl = make_state_table () }
  in
  let seeds =
    List.map
      (fun r -> { node = r.entry; tags = TagMap.empty })
      (Array.to_list rules)
  in
  let num0, init_tags = get_state ctx (closure seeds) in
  assert (num0 = 0);
  {
    dfa = Array.init ctx.tbl.n_states (Hashtbl.find ctx.tbl.defs);
    init_tags;
    num_tags = ctx.regs.next_cell;
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
