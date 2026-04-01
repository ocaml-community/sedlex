(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

open Ppxlib
open Ast_builder.Default
open Ast_helper

(* let ocaml_version = Versions.ocaml_408 *)

module Cset = Sedlex_cset

(* Decision tree for partitions.

   A partition maps Unicode code points to equivalence class indices. Rather
   than generating a flat lookup table (which would be huge), we build a
   binary decision tree that tests code points against split values. For
   dense regions below [limit], a compact byte-string table is used instead.
   [simplify] prunes unreachable branches given a known code-point range. *)

let default_loc = Location.none

type decision_tree =
  | Lte of int * decision_tree * decision_tree
  | Table of int * int array
  | Return of int

let rec simplify_decision_tree (x : decision_tree) =
  match x with
    | Table _ | Return _ -> x
    | Lte (_, (Return a as l), Return b) when a = b -> l
    | Lte (i, l, r) -> (
        let l = simplify_decision_tree l in
        let r = simplify_decision_tree r in
        match (l, r) with
          | Return a, Return b when a = b -> l
          | _ -> Lte (i, l, r))

(* [decision segments] builds a balanced binary decision tree from a sorted
   list of [(lo, hi, class_index)] segments. Pairs of adjacent segments are
   merged bottom-up into [Lte] nodes. Gaps between segments return -1
   (no match). *)
let decision l =
  let l = List.map (fun (a, b, i) -> (a, b, Return i)) l in
  let rec merge2 = function
    | (a1, b1, d1) :: (a2, b2, d2) :: rest ->
        let x = if b1 + 1 = a2 then d2 else Lte (a2 - 1, Return (-1), d2) in
        (a1, b2, Lte (b1, d1, x)) :: merge2 rest
    | rest -> rest
  in
  let rec aux = function
    | [(a, b, d)] -> Lte (a - 1, Return (-1), Lte (b, d, Return (-1)))
    | [] -> Return (-1)
    | l -> aux (merge2 l)
  in
  aux l

(* Code points below [limit] with class index < 255 are eligible for
   compact byte-string table lookup instead of a decision tree. *)
let limit = 8192

(* [decision_table segments] partitions segments into a table-eligible
   prefix (dense, low code points) and a tree-handled suffix. The prefix
   becomes a [Table] node for O(1) lookup; the suffix uses [decision]. *)
let decision_table l =
  let rec aux m accu = function
    | ((a, b, i) as x) :: rem when b < limit && i < 255 ->
        aux (min a m) (x :: accu) rem
    | rem -> (m, accu, rem)
  in
  let min, table, rest = aux max_int [] l in
  match table with
    | [] -> decision l
    | [(min, max, i)] ->
        Lte (min - 1, Return (-1), Lte (max, Return i, decision rest))
    | (_, max, _) :: _ ->
        let arr = Array.make (max - min + 1) 0 in
        let set (a, b, i) =
          for j = a to b do
            arr.(j - min) <- i + 1
          done
        in
        List.iter set table;
        Lte (min - 1, Return (-1), Lte (max, Table (min, arr), decision rest))

let rec simplify min max = function
  | Lte (i, yes, no) ->
      if i >= max then simplify min max yes
      else if i < min then simplify min max no
      else Lte (i, simplify min i yes, simplify (i + 1) max no)
  | x -> x

(* [segments_of_partition p] flattens a partition (array of char sets, one
   per equivalence class) into a sorted list of [(lo, hi, class_index)]
   segments suitable for [decision_table]. *)
let segments_of_partition p =
  let seg = ref [] in
  Array.iteri
    (fun i c ->
      List.iter
        (fun (a, b) -> seg := (a, b, i) :: !seg)
        (c : Sedlex_cset.t :> (int * int) list))
    p;
  List.sort (fun (a1, _, _) (a2, _, _) -> compare a1 a2) !seg

(* [decision_table partition] builds a complete decision tree for a
   partition: extracts segments, builds the hybrid table/tree, then
   simplifies by pruning branches outside the valid code-point range. *)
let decision_table p =
  simplify (-1) Cset.max_code (decision_table (segments_of_partition p))

(* Helpers to build AST *)

let appfun s l =
  let loc = default_loc in
  eapply ~loc (evar ~loc s) l

let glb_value name def =
  let loc = default_loc in
  pstr_value ~loc Nonrecursive
    [value_binding ~loc ~pat:(pvar ~loc name) ~expr:def]

(* Named regexps *)

module StringMap = Map.Make (struct
  type t = string

  let compare = compare
end)

let builtin_regexps =
  List.fold_left
    (fun acc (n, c) ->
      StringMap.add n (Sedlex.chars c, Some 1 (* fixed length *)) acc)
    StringMap.empty
    ([
       ("any", Cset.any);
       ("eof", Cset.eof);
       ("xml_letter", Xml.letter);
       ("xml_digit", Xml.digit);
       ("xml_extender", Xml.extender);
       ("xml_base_char", Xml.base_char);
       ("xml_ideographic", Xml.ideographic);
       ("xml_combining_char", Xml.combining_char);
       ("xml_blank", Xml.blank);
       ("tr8876_ident_char", Iso.tr8876_ident_char);
     ]
    @ Unicode.Categories.list @ Unicode.Properties.list)

(* Tables (indexed mapping: codepoint -> next state) *)

let tables = Hashtbl.create 31
let get_tables () = Hashtbl.fold (fun key x accu -> (x, key) :: accu) tables []

let table_name x =
  try Hashtbl.find tables x
  with Not_found ->
    let s = Printf.sprintf "__sedlex_table_%i" (Hashtbl.length tables + 1) in
    Hashtbl.add tables x s;
    s

(* [table (name, v)] generates a top-level [let __sedlex_table_N = "..."]
   binding where the string encodes the byte array [v] (one byte per entry,
   used for compact partition lookup). *)
let table (name, v) =
  let n = Array.length v in
  let s = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.set s i (Char.chr v.(i))
  done;
  glb_value name (estring ~loc:default_loc (Bytes.to_string s))

(* Partition (function: codepoint -> next state) *)

let partitions = Hashtbl.create 31

let get_partitions () =
  Hashtbl.fold (fun key x accu -> (x, key) :: accu) partitions []

let partition_name x =
  try Hashtbl.find partitions x
  with Not_found ->
    let s =
      Printf.sprintf "__sedlex_partition_%i" (Hashtbl.length partitions + 1)
    in
    Hashtbl.add partitions x s;
    s

let reset_state () =
  Hashtbl.clear tables;
  Hashtbl.clear partitions

(* [partition (name, p)] generates a top-level [let __sedlex_partition_N c = ...]
   function that maps a code point [c] to its equivalence class index using
   the decision tree built from partition [p]. The EOF case (-1) is handled
   naturally by the decision tree since [simplify] uses -1 as the lower bound. *)
let partition (name, p) =
  let loc = default_loc in
  let rec gen_tree = function
    | Lte (i, yes, no) ->
        [%expr
          if c <= [%e eint ~loc i] then [%e gen_tree yes] else [%e gen_tree no]]
    | Return i -> eint ~loc:default_loc i
    | Table (offset, t) ->
        let c =
          if offset = 0 then [%expr c] else [%expr c - [%e eint ~loc offset]]
        in
        [%expr
          Char.code (String.unsafe_get [%e evar ~loc (table_name t)] [%e c]) - 1]
  in
  let body = gen_tree (simplify_decision_tree (decision_table p)) in
  glb_value name
    [%expr
      fun c ->
        let open! Stdlib in
        [%e body]]

(* Code generation for the automata *)

(* [best_final finals] returns the lowest-numbered accepting rule for this
   state, or [None] if the state is not accepting. Lowest-numbered = highest
   priority, matching the first-match semantics of [match%sedlex]. *)
let best_final final =
  let fin = ref None in
  for i = Array.length final - 1 downto 0 do
    if final.(i) then fin := Some i
  done;
  !fin

let state_fun state = Printf.sprintf "__sedlex_state_%i" state

(* [call_state lexbuf auto state] generates the expression that transitions
   into DFA [state]. If the state has no outgoing transitions (a sink), it
   returns the accepting rule index directly; otherwise it emits a function
   call to the generated state function. *)
let call_state lexbuf (auto : Sedlex.dfa) state =
  let { Sedlex.trans; finals } = auto.(state) in
  if Array.length trans = 0 then (
    match best_final finals with
      | Some i -> eint ~loc:default_loc i
      | None -> assert false)
  else appfun (state_fun state) [lexbuf]

(* [gen_tag_ops lexbuf ops cont] wraps [cont] in a sequence of tag
   operation calls. Each [Set_position t] becomes a call to
   [__private__set_mem_pos], and each [Set_value (cell, v)] becomes a call to
   [__private__set_mem_value]. Operations are folded right so they execute
   before [cont]. *)
let gen_tag_ops lexbuf (ops : Sedlex.tag_op list) cont =
  let loc = default_loc in
  List.fold_right
    (fun (op : Sedlex.tag_op) acc ->
      match op with
        | Set_position t ->
            [%expr
              Sedlexing.__private__set_mem_pos [%e lexbuf] [%e eint ~loc t];
              [%e acc]]
        | Set_value (cell, value) ->
            [%expr
              Sedlexing.__private__set_mem_value [%e lexbuf] [%e eint ~loc cell]
                [%e eint ~loc value];
              [%e acc]])
    ops cont

(* [gen_state (lexbuf_name, lexbuf) auto i {trans; finals}] generates the
   function [__sedlex_state_N] for DFA state [i]. The function:
   1. If the state is accepting, calls [mark] to save the current position.
   2. Reads the next code point, maps it through the partition function to
      get an equivalence class index, then pattern-matches on that index.
   3. Each transition arm executes its tag operations then calls the target
      state function (or returns the rule index for sink states).
   4. The default arm calls [backtrack] to return the last accepted rule.
   Returns [] for accepting states with no outgoing transitions (sinks). *)
let gen_state (lexbuf_name, lexbuf) (auto : Sedlex.dfa) i
    { Sedlex.trans; finals } =
  let loc = default_loc in
  let partition = Array.map (fun (cs, _, _) -> cs) trans in
  let cases =
    Array.mapi
      (fun i (_, j, tags) ->
        let rhs = gen_tag_ops lexbuf tags (call_state lexbuf auto j) in
        case ~lhs:(pint ~loc i) ~guard:None ~rhs)
      trans
  in
  let cases = Array.to_list cases in
  let body () =
    pexp_match ~loc
      (appfun (partition_name partition)
         [[%expr Sedlexing.__private__next_int [%e lexbuf]]])
      (cases
      @ [
          case
            ~lhs:[%pat? _]
            ~guard:None
            ~rhs:[%expr Sedlexing.backtrack [%e lexbuf]];
        ])
  in
  let ret body =
    let lhs = pvar ~loc:lexbuf.pexp_loc lexbuf_name in
    [
      value_binding ~loc
        ~pat:(pvar ~loc (state_fun i))
        ~expr:(Exp.fun_ ~loc Nolabel None lhs body);
    ]
  in
  match best_final finals with
    | None -> ret (body ())
    | Some _ when Array.length trans = 0 -> []
    | Some i ->
        ret
          [%expr
            Sedlexing.mark [%e lexbuf] [%e eint ~loc i];
            [%e body ()]]

(* [gen_recflag auto] determines whether the generated state functions need
   [let rec]. If every transition leads to a sink state (no further
   transitions), the functions are non-recursive; otherwise they are
   mutually recursive. *)
let gen_recflag (auto : Sedlex.dfa) =
  (* The generated function is not recursive if the transitions end
     in states with no further transitions. *)
  try
    Array.iter
      (fun { Sedlex.trans; _ } ->
        Array.iter
          (fun (_, j, _) ->
            if Array.length auto.(j).Sedlex.trans > 0 then raise Exit)
          trans)
      auto;
    Nonrecursive
  with Exit -> Recursive

(* [gen_definition lexbuf_with_name compiled cases error] generates the
   complete lexer expression for one [match%sedlex] block:
   - Defines all [__sedlex_state_N] functions via [let rec ... in].
   - Emits a start sequence: [start lexbuf], then (if the pattern has [as]
     bindings) [init_mem] + initial tag operations, then calls state 0.
   - Wraps the result in a [match] on the returned rule index, dispatching
     to user-provided right-hand-side expressions, with [error] as default. *)
let gen_definition ((_, lexbuf) as lexbuf_with_name)
    (compiled : Sedlex.compiled) l error =
  let loc = default_loc in
  let auto = compiled.dfa in
  let cases =
    List.mapi (fun i (_, e) -> case ~lhs:(pint ~loc i) ~guard:None ~rhs:e) l
  in
  let states = Array.mapi (gen_state lexbuf_with_name auto) auto in
  let states = List.flatten (Array.to_list states) in
  let start_expr =
    if compiled.num_tags > 0 then (
      let init_mem =
        [%expr
          Sedlexing.__private__init_mem [%e lexbuf]
            [%e eint ~loc compiled.num_tags]]
      in
      let set_init_tags =
        gen_tag_ops lexbuf compiled.init_tags (appfun (state_fun 0) [lexbuf])
      in
      pexp_sequence ~loc
        [%expr Sedlexing.start [%e lexbuf]]
        (pexp_sequence ~loc init_mem set_init_tags))
    else
      pexp_sequence ~loc
        [%expr Sedlexing.start [%e lexbuf]]
        (appfun (state_fun 0) [lexbuf])
  in
  pexp_let ~loc (gen_recflag auto) states
    (pexp_match ~loc start_expr
       (cases @ [case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:error]))

(* Lexer specification parser *)

let codepoint i =
  if i < 0 || i > Cset.max_code then
    failwith (Printf.sprintf "Invalid Unicode code point: %i" i);
  i

let char c = Cset.singleton (Char.code c)
let uchar c = Cset.singleton (Uchar.to_int c)

let err loc fmt =
  Printf.ksprintf
    (fun s ->
      raise (Location.Error (Location.Error.createf ~loc "Sedlex: %s" s)))
    fmt

type encoding = Utf8 | Latin1 | Ascii

let string_of_encoding = function
  | Utf8 -> "UTF-8"
  | Latin1 -> "Latin-1"
  | Ascii -> "ASCII"

(* [rev_csets_of_string ~loc ~encoding s] decodes string [s] under
   [encoding] and returns a list of singleton char sets in reverse order
   (one per character). Used to build sequence regexps from string literals. *)
let rev_csets_of_string ~loc ~encoding s =
  match encoding with
    | Utf8 ->
        Utf8.fold
          ~f:(fun acc _ x ->
            match x with
              | `Malformed _ ->
                  err loc "Malformed %s string" (string_of_encoding encoding)
              | `Uchar c -> uchar c :: acc)
          [] s
    | Latin1 ->
        let l = ref [] in
        for i = 0 to String.length s - 1 do
          l := char s.[i] :: !l
        done;
        !l
    | Ascii ->
        let l = ref [] in
        for i = 0 to String.length s - 1 do
          match s.[i] with
            | '\x00' .. '\x7F' as c -> l := char c :: !l
            | _ -> err loc "Malformed %s string" (string_of_encoding encoding)
        done;
        !l

(* [repeat r (n, m)] expands bounded repetition [Rep(r, n..m)] into a
   sequence of [n] mandatory copies followed by [m - n] optional copies. *)
let rec repeat r = function
  | 0, 0 -> Sedlex.eps
  | 0, m -> Sedlex.alt Sedlex.eps (Sedlex.seq r (repeat r (0, m - 1)))
  | n, m -> Sedlex.seq r (repeat r (n - 1, m - 1))

(* Code generation for `as` bindings.

   [regexp_of_pattern] parses OCaml patterns into regexps and collects a
   [tag_info list] for every [as] binding it encounters. Each [tag_info]
   records the variable name and a [pos_expr] for its start/end positions.
   For or-patterns like [(p1 as x) | (p2 as x)], each branch gets its own
   positions and a discriminator [(cell, value)] so the generated code can
   determine which branch matched.

   Position expressions ([pos_expr]) allow sub-match boundaries to be
   computed without memory cells when possible. [fixed_length] determines
   if a pattern has a statically known length; [advance] and [retreat]
   propagate known positions through concatenation. For example, in
   ['a', ((Plus 'b') as x)], the start of [x] is [Start_plus 1] (one
   code point from the token start), so no start tag is needed.

   [gen_binding_code] turns the [tag_info list] into [let] bindings that
   extract sub-matches from the lexbuf's memory cells (or computed offsets).
   For or-patterns with discriminators it emits a chain of if/else
   checks on the discriminator cell to select the correct positions. *)

(* A [pos_expr] represents a sub-match boundary position that may be
   computed without a memory cell:
   - [Tag {tag; offset}]: read memory cell [tag] and add [offset].
   - [Start_plus n]: the position is [n] code points from the token start.
   - [End_minus n]: the position is [n] code points before the token end.
   When both boundaries of an [as] binding can be expressed as [Start_plus]
   or [End_minus], no memory cells are needed at all. *)
type pos_expr =
  | Tag of { tag : int; offset : int }
  | Start_plus of int
  | End_minus of int

type tag_info = {
  name : string;
  start_pos : pos_expr;
  end_pos : pos_expr;
  disc : (int * int) list;
      (* Discriminator conditions: [(cell, value)] pairs. For simple
         bindings this is [[]]. For or-patterns, each branch has a
         distinct value in the shared discriminator cell. *)
}

(* [advance pe len] shifts a position expression forward by [len] code
   points. Returns [None] if either argument is unknown. *)
let advance pe len =
  match (pe, len) with
    | Some (Start_plus n), Some l -> Some (Start_plus (n + l))
    | Some (End_minus n), Some l -> Some (End_minus (n - l))
    | Some (Tag { tag; offset }), Some l ->
        Some (Tag { tag; offset = offset + l })
    | _ -> None

(* [retreat pe len] shifts a position expression backward by [len] code
   points. Returns [None] if either argument is unknown. *)
let retreat pe len =
  match (pe, len) with
    | Some (End_minus n), Some l -> Some (End_minus (n + l))
    | Some (Start_plus n), Some l -> Some (Start_plus (n - l))
    | Some (Tag { tag; offset }), Some l ->
        Some (Tag { tag; offset = offset - l })
    | _ -> None

(* [gen_pos_expr lexbuf pe] generates code that evaluates a [pos_expr]
   to an integer position (offset from token start, in code points). *)
let gen_pos_expr lexbuf pe =
  let loc = default_loc in
  match pe with
    | Tag { tag; offset = 0 } ->
        [%expr Sedlexing.__private__mem_pos [%e lexbuf] [%e eint ~loc tag]]
    | Tag { tag; offset } ->
        [%expr
          Sedlexing.__private__mem_pos [%e lexbuf] [%e eint ~loc tag]
          + [%e eint ~loc offset]]
    | Start_plus n -> eint ~loc n
    | End_minus 0 -> [%expr Sedlexing.lexeme_length [%e lexbuf]]
    | End_minus n ->
        [%expr Sedlexing.lexeme_length [%e lexbuf] - [%e eint ~loc n]]

(* [gen_sub_lexeme lexbuf st et] generates an expression that evaluates
   position expressions [st] and [et] and constructs a [Sedlexing.submatch]
   record. *)
let gen_sub_lexeme lexbuf st et =
  let loc = default_loc in
  [%expr
    let __s = [%e gen_pos_expr lexbuf st] in
    let __e = [%e gen_pos_expr lexbuf et] in
    { Sedlexing.lexbuf = [%e lexbuf]; pos = __s; len = __e - __s }]

(* [gen_binding_code lexbuf tag_info action] wraps [action] with [let]
   bindings that extract sub-match values from the lexbuf's memory cells.
   For a single binding (no or-pattern), emits a direct sub_lexeme call.
   For or-patterns with multiple tag pairs, emits a chain of
   [if disc_cell = value then ...] to select the correct positions at runtime. *)
let gen_binding_code lexbuf (tag_info : tag_info list) action =
  let loc = default_loc in
  ignore loc;
  if tag_info = [] then action
  else (
    (* Group tag_info by variable name *)
    let by_name =
      let tbl = Hashtbl.create 8 in
      let order = ref [] in
      List.iter
        (fun { name; start_pos; end_pos; disc } ->
          if not (Hashtbl.mem tbl name) then order := name :: !order;
          let existing = try Hashtbl.find tbl name with Not_found -> [] in
          Hashtbl.replace tbl name (existing @ [(start_pos, end_pos, disc)]))
        tag_info;
      List.rev_map (fun name -> (name, Hashtbl.find tbl name)) !order
    in
    List.fold_right
      (fun (name, entries) acc ->
        match entries with
          | [(st, et, _)] ->
              [%expr
                let [%p pvar ~loc name] = [%e gen_sub_lexeme lexbuf st et] in
                [%e acc]]
          | _ ->
              let gen_disc_cond discs =
                let check (cell, value) =
                  [%expr
                    Sedlexing.__private__mem_value [%e lexbuf]
                      [%e eint ~loc cell]
                    = [%e eint ~loc value]]
                in
                match discs with
                  | [] -> assert false
                  | [d] -> check d
                  | d :: ds ->
                      List.fold_left
                        (fun acc d -> [%expr [%e acc] && [%e check d]])
                        (check d) ds
              in
              let rec gen_checks = function
                | [(st, et, _)] -> gen_sub_lexeme lexbuf st et
                | (st, et, discs) :: rest ->
                    if discs = [] then
                      failwith
                        "discriminator required for or-pattern with multiple \
                         bindings";
                    [%expr
                      if [%e gen_disc_cond discs] then
                        [%e gen_sub_lexeme lexbuf st et]
                      else [%e gen_checks rest]]
                | [] -> assert false
              in
              [%expr
                let [%p pvar ~loc name] = [%e gen_checks entries] in
                [%e acc]])
      by_name action)

(* [codepoint_count ~encoding s] returns the number of Unicode code points
   in string [s] under the given encoding. *)
let codepoint_count ~encoding s =
  match encoding with
    | Latin1 | Ascii -> String.length s
    | Utf8 ->
        let n = ref 0 in
        String.iter (fun c -> if Char.code c land 0xC0 <> 0x80 then incr n) s;
        !n

(* [fixed_length env ~encoding p] returns [Some n] if pattern [p] always
   matches exactly [n] code points, or [None] if the length is variable.
   Used to compute [Start_plus]/[End_minus] offsets for [as] bindings. *)
let rec fixed_length env ~encoding p =
  match p.ppat_desc with
    | Ppat_alias (inner, _) -> fixed_length env ~encoding inner
    | Ppat_or (p1, p2) -> (
        match
          (fixed_length env ~encoding p1, fixed_length env ~encoding p2)
        with
          | Some n1, Some n2 when n1 = n2 -> Some n1
          | _ -> None)
    | Ppat_tuple (p :: pl) ->
        List.fold_left
          (fun acc p ->
            match (acc, fixed_length env ~encoding p) with
              | Some a, Some b -> Some (a + b)
              | _ -> None)
          (fixed_length env ~encoding p)
          pl
    | Ppat_constant (Pconst_string (s, _, _)) ->
        Some (codepoint_count ~encoding s)
    | Ppat_constant (Pconst_char _) -> Some 1
    | Ppat_constant (Pconst_integer _) -> Some 1
    | Ppat_interval _ -> Some 1
    | Ppat_construct ({ txt = Lident "Chars"; _ }, _) -> Some 1
    | Ppat_construct ({ txt = Lident "Compl"; _ }, _) -> Some 1
    | Ppat_construct ({ txt = Lident "Sub"; _ }, _) -> Some 1
    | Ppat_construct ({ txt = Lident "Intersect"; _ }, _) -> Some 1
    | Ppat_construct ({ txt = Lident "Utf8"; _ }, Some (_, p)) ->
        fixed_length env ~encoding:Utf8 p
    | Ppat_construct ({ txt = Lident "Latin1"; _ }, Some (_, p)) ->
        fixed_length env ~encoding:Latin1 p
    | Ppat_construct ({ txt = Lident "Ascii"; _ }, Some (_, p)) ->
        fixed_length env ~encoding:Ascii p
    | Ppat_construct
        ( { txt = Lident "Rep"; _ },
          Some
            ( _,
              {
                ppat_desc =
                  Ppat_tuple
                    [
                      p0;
                      {
                        ppat_desc =
                          Ppat_constant (i1 as i2) | Ppat_interval (i1, i2);
                        _;
                      };
                    ];
                _;
              } ) ) -> (
        match (i1, i2) with
          | Pconst_integer (i1, _), Pconst_integer (i2, _) when i1 = i2 -> (
              match fixed_length env ~encoding p0 with
                | Some l -> Some (int_of_string i1 * l)
                | None -> None)
          | _ -> None)
    | Ppat_var { txt = x; _ } -> (
        match StringMap.find_opt x env with
          | Some (_, len) -> len
          | None -> None)
    | _ -> None

(* [regexp_of_pattern env pattern] parses an OCaml pattern AST into a
   [Sedlex.regexp] and a [tag_info list] for any [as] bindings encountered.
   [env] maps names to previously defined regexps (built-in + user-defined).
   Handles all sedlex pattern constructors: literals, Star, Plus, Rep, Opt,
   Compl, Sub, Intersect, Chars, character intervals, tuple (sequence),
   or-patterns, and [Ppat_alias] for [as] bindings. *)
let regexp_of_pattern env =
  (* Anchors: [(start_anchor, end_anchor)] propagated upward by [aux].
     Tuples and aliases produce anchors; an enclosing [Ppat_alias]
     uses them as fallback positions when [left]/[right] are unknown.
     The general principle is "prefer the tag that fires latest" so
     that tag writes are delayed as long as possible. *)
  let no_anchors = (None, None) in
  let no_tags r = (r, ([] : tag_info list), no_anchors) in
  (* [prefer ~best a b]: pick the best position expression.
     [Start_plus]/[End_minus] always beat [Tag] (no runtime tag needed).
     [~best] selects which tag-free form is preferred: [`Start] for
     start boundaries (a plain constant), [`End] for end boundaries
     (relative to token end).  Among [Tag] values, [a] wins (caller
     controls priority by argument order). *)
  let prefer ~best a b =
    let is_best = function
      | Some (Start_plus _) -> best = `Start
      | Some (End_minus _) -> best = `End
      | _ -> false
    in
    match (a, b) with
      | s, _ when is_best s -> s
      | _, s when is_best s -> s
      | (Some (Start_plus _ | End_minus _) as s), _ -> s
      | _, (Some (Start_plus _ | End_minus _) as s) -> s
      | Some _, _ -> a
      | _ -> b
  in
  let reject_tags loc ctx (r, tags, _anchors) =
    if tags <> [] then err loc "'as' bindings are not supported inside %s" ctx;
    r
  in
  let rec char_pair_op func name ~encoding ~loc tuple =
    (* Construct something like Sub(a,b) *)
      match tuple with
      | Some { ppat_desc = Ppat_tuple [p0; p1]; _ } -> begin
          let r0 =
            reject_tags p0.ppat_loc name
              (aux ~left:None ~right:None ~encoding p0)
          in
          let r1 =
            reject_tags p1.ppat_loc name
              (aux ~left:None ~right:None ~encoding p1)
          in
          match func r0 r1 with
            | Some r -> no_tags r
            | None ->
                err loc
                  "the %s operator can only applied to single-character length \
                   regexps"
                  name
        end
      | _ ->
          err loc "the %s operator requires two arguments, like %s(a,b)" name
            name
  and aux ~left ~right ~encoding p =
    (* [left]: known position at the start of this pattern element.
       [right]: known position at the end of this pattern element.
       Both are [pos_expr option]: any variant is possible, or [None]
       when the position cannot be determined statically.

       Returns [(regexp, tag_info list, (start_anchor, end_anchor))].
       The anchors provide position information for an enclosing
       [Ppat_alias] when its own [left]/[right] context is unknown:
       - [start_anchor]: best-known position before this pattern.
         For tuples, this combines the rights-computation [pre_start]
         (derived from outer right-side tags) with the first element's
         inner anchor.  For aliases, this is [st] (the chosen start).
       - [end_anchor]: best-known position after this pattern.
         For tuples, this combines [rights.(n-1)] (from outer right)
         with the last element's inner anchor.  For aliases, this is
         [et] (the chosen end).
       Leaf patterns and combinators return [(None, None)]. *)
      match p.ppat_desc with
      (* name as x — named sub-match binding.
         Try to derive each boundary from [left]/[right] context or
         [fixed_length]; allocate tags only for boundaries that cannot
         be computed statically. Best case: 0 tags. Worst case: 2. *)
      | Ppat_alias (inner, { txt = name; _ }) ->
          let r, tags, (start_anchor, end_anchor) =
            aux ~left ~right ~encoding inner
          in
          let elem_len = fixed_length env ~encoding inner in
          (* Compute known boundaries.  General principle: prefer the
             expression that fires latest (delays the tag write), with
             [Start_plus]/[End_minus] always winning (no tag needed).
             For tags, later-firing is better:
             - [known_start]: right retreat > inner anchor > outer left
               (right tag fires past alias, inner fires inside, left
               fires at start).
             - [known_end]: outer right > end anchor > start advance
               (right fires past alias, end anchor fires at alias end,
               start advance depends on which tag known_start chose). *)
          let from_left = left in
          let from_right_retreat = retreat right elem_len in
          let known_start =
            prefer ~best:`Start from_right_retreat
              (prefer ~best:`Start start_anchor from_left)
          in
          let from_right = right in
          let from_start_advance = advance known_start elem_len in
          let known_end =
            prefer ~best:`End from_right
              (prefer ~best:`End end_anchor from_start_advance)
          in
          let st, et, r =
            match (known_start, known_end) with
              | Some st, Some et -> (st, et, r)
              | Some st, None ->
                  let wrapped, end_tag = Sedlex.bind_end_only r in
                  (st, Tag { tag = end_tag; offset = 0 }, wrapped)
              | None, Some et ->
                  let wrapped, start_tag = Sedlex.bind_start_only r in
                  (Tag { tag = start_tag; offset = 0 }, et, wrapped)
              | None, None -> (
                  match elem_len with
                    | Some len ->
                        let wrapped, end_tag = Sedlex.bind_end_only r in
                        ( Tag { tag = end_tag; offset = -len },
                          Tag { tag = end_tag; offset = 0 },
                          wrapped )
                    | None ->
                        let wrapped, start_tag, end_tag = Sedlex.bind r in
                        ( Tag { tag = start_tag; offset = 0 },
                          Tag { tag = end_tag; offset = 0 },
                          wrapped ))
          in
          (* Propagate [st]/[et] as anchors: they incorporate inner
             anchors and any tags the alias allocated, so a further
             enclosing alias can reuse them. *)
          ( r,
            { name; start_pos = st; end_pos = et; disc = [] } :: tags,
            (Some st, Some et) )
      (* p1 | p2 — alternation *)
      | Ppat_or (p1, p2) ->
          let r1, tags1, (sa1, ea1) = aux ~left ~right ~encoding p1 in
          let r2, tags2, (sa2, ea2) = aux ~left ~right ~encoding p2 in
          (* Propagate anchors only when both branches agree — this
             happens for [Start_plus]/[End_minus] derived from the
             same outer context.  Tag-based anchors will differ
             across branches (independent boundary tag allocations). *)
          let merge a1 a2 = if a1 = a2 then a1 else None in
          let anchors = (merge sa1 sa2, merge ea1 ea2) in
          if tags1 <> [] || tags2 <> [] then begin
            let names tags =
              List.map (fun ti -> ti.name) tags |> List.sort_uniq String.compare
            in
            if names tags1 <> names tags2 then
              err p.ppat_loc
                "both sides of '|' must bind the same names with 'as'";
            (* When both branches produce identical positions (e.g.
               same fixed-length elements), no discriminator is needed. *)
            if tags1 = tags2 then (Sedlex.alt r1 r2, tags1, anchors)
            else (
              let stamp disc_cell value tags =
                List.map
                  (fun ti -> { ti with disc = (disc_cell, value) :: ti.disc })
                  tags
              in
              (* Discriminator cell reuse: OCaml desugars
                 [a | b | c] into [(a | b) | c], so by the time we
                 see the outer [|], the left branch already carries a
                 discriminator cell from the inner [|].  We reuse that
                 cell for the new right branch by assigning the next
                 unused value, keeping exactly one discriminator cell
                 per group of alternatives regardless of arity. *)
              let reusable_cell =
                match tags1 with
                  | [] | { disc = []; _ } :: _ -> None
                  | { disc = (c, _) :: _; _ } :: rest ->
                      if
                        List.for_all
                          (fun ti ->
                            match ti.disc with
                              | (c2, _) :: _ -> c2 = c
                              | [] -> false)
                          rest
                      then Some c
                      else None
              in
              match reusable_cell with
                | Some disc_cell ->
                    let max_val =
                      List.fold_left
                        (fun acc ti ->
                          match ti.disc with
                            | (_, v) :: _ -> max acc v
                            | [] -> acc)
                        (-1) tags1
                    in
                    let new_val = max_val + 1 in
                    let r2w = Sedlex.bind_disc r2 disc_cell new_val in
                    ( Sedlex.alt r1 r2w,
                      tags1 @ stamp disc_cell new_val tags2,
                      anchors )
                | None ->
                    let disc_cell = Sedlex.new_disc_cell () in
                    let r1w = Sedlex.bind_disc r1 disc_cell 0 in
                    let r2w = Sedlex.bind_disc r2 disc_cell 1 in
                    ( Sedlex.alt r1w r2w,
                      stamp disc_cell 0 tags1 @ stamp disc_cell 1 tags2,
                      anchors ))
          end
          else (Sedlex.alt r1 r2, tags1 @ tags2, anchors)
      (* (p1, p2, ...) — sequence *)
      | Ppat_tuple (p :: pl) ->
          let all = p :: pl in
          let n = List.length all in
          let lengths = List.map (fun p -> fixed_length env ~encoding p) all in
          let lengths_arr = Array.of_list lengths in
          (* Compute right positions (right-to-left).
             When [retreat] breaks (variable-length element or unknown
             [right]) but the element has fixed length, allocate a
             boundary tag at the element's end.  This tag becomes a
             concrete anchor: subsequent elements (to the left) can
             compute their right positions via [retreat] from the tag.
             The tag is placed in the NFA during the fold via
             [Sedlex.tag_end]. *)
          let boundary_tags = Array.make n None in
          let rights = Array.make n None in
          let pre_start =
            let acc = ref right in
            for i = n - 1 downto 0 do
              match (!acc, lengths_arr.(i)) with
                | _, None ->
                    rights.(i) <- !acc;
                    acc := None
                | None, Some _ ->
                    let tag = Sedlex.new_disc_cell () in
                    boundary_tags.(i) <- Some tag;
                    let tag_pos = Some (Tag { tag; offset = 0 }) in
                    rights.(i) <- tag_pos;
                    acc := retreat tag_pos lengths_arr.(i)
                | Some _, Some _ ->
                    rights.(i) <- !acc;
                    acc := retreat !acc lengths_arr.(i)
            done;
            !acc
          in
          (* [update_left cur i ea]: compute the left position for the
             next element after processing element [i].
             1. [advance] — known left + fixed-length element
             2. [ea] — end anchor from the element (alias end tag,
                tuple boundary anchor, etc.)
             3. [boundary_tags.(i)] — boundary tag from rights pass *)
          let update_left cur i ea =
            match advance cur lengths_arr.(i) with
              | Some _ as s -> s
              | None -> (
                  match ea with
                    | Some _ -> ea
                    | None -> (
                        match boundary_tags.(i) with
                          | Some tag -> Some (Tag { tag; offset = 0 })
                          | None -> None))
          in
          let r0, tags0, (start0, end0) =
            aux ~left ~right:rights.(0) ~encoding p
          in
          let r0 =
            match boundary_tags.(0) with
              | Some tag -> Sedlex.tag_end tag r0
              | None -> r0
          in
          let left0 = update_left left 0 end0 in
          let _, _, result =
            List.fold_left
              (fun (i, cur_left, (r, tags, _prev_end)) p ->
                let r', tags', (_sa, ea) =
                  aux ~left:cur_left ~right:rights.(i) ~encoding p
                in
                let r' =
                  match boundary_tags.(i) with
                    | Some tag -> Sedlex.tag_end tag r'
                    | None -> r'
                in
                let new_left = update_left cur_left i ea in
                (i + 1, new_left, (Sedlex.seq r r', tags @ tags', ea)))
              (1, left0, (r0, tags0, end0))
              pl
          in
          let r, tags, last_end = result in
          (* Combine outer and inner anchors, preferring the tag that
             fires latest.  [pre_start] / [rights.(n-1)] come from the
             rights computation and may reference outer boundary tags
             (placed after the tuple in the enclosing sequence).
             [start0] / [last_end] come from the first/last element's
             own processing and reference inner tags. *)
          let end_anchor = prefer ~best:`End rights.(n - 1) last_end in
          let start_anchor = prefer ~best:`Start pre_start start0 in
          (r, tags, (start_anchor, end_anchor))
      (* Star p — zero-or-more repetition *)
      | Ppat_construct ({ txt = Lident "Star"; _ }, Some (_, p)) ->
          let r =
            reject_tags p.ppat_loc "Star"
              (aux ~left:None ~right:None ~encoding p)
          in
          no_tags (Sedlex.rep r)
      (* Plus p — one-or-more repetition *)
      | Ppat_construct ({ txt = Lident "Plus"; _ }, Some (_, p)) ->
          let r =
            reject_tags p.ppat_loc "Plus"
              (aux ~left:None ~right:None ~encoding p)
          in
          no_tags (Sedlex.plus r)
      (* Utf8 p — switch to UTF-8 encoding *)
      | Ppat_construct ({ txt = Lident "Utf8"; _ }, Some (_, p)) ->
          aux ~left ~right ~encoding:Utf8 p
      (* Latin1 p — switch to Latin-1 encoding *)
      | Ppat_construct ({ txt = Lident "Latin1"; _ }, Some (_, p)) ->
          aux ~left ~right ~encoding:Latin1 p
      (* Ascii p — switch to ASCII encoding *)
      | Ppat_construct ({ txt = Lident "Ascii"; _ }, Some (_, p)) ->
          aux ~left ~right ~encoding:Ascii p
      (* Rep (p, n..m) — bounded repetition *)
      | Ppat_construct
          ( { txt = Lident "Rep"; _ },
            Some
              ( _,
                {
                  ppat_desc =
                    Ppat_tuple
                      [
                        p0;
                        {
                          ppat_desc =
                            Ppat_constant (i1 as i2) | Ppat_interval (i1, i2);
                          _;
                        };
                      ];
                  _;
                } ) ) -> begin
          let r =
            reject_tags p0.ppat_loc "Rep"
              (aux ~left:None ~right:None ~encoding p0)
          in
          match (i1, i2) with
            | Pconst_integer (i1, _), Pconst_integer (i2, _) ->
                let i1 = int_of_string i1 in
                let i2 = int_of_string i2 in
                if 0 <= i1 && i1 <= i2 then no_tags (repeat r (i1, i2))
                else err p.ppat_loc "Invalid range for Rep operator"
            | _ ->
                err p.ppat_loc "Rep must take an integer constant or interval"
        end
      (* Rep _ — malformed Rep *)
      | Ppat_construct ({ txt = Lident "Rep"; _ }, _) ->
          err p.ppat_loc "the Rep operator takes 2 arguments"
      (* Opt p — optional (zero or one) *)
      | Ppat_construct ({ txt = Lident "Opt"; _ }, Some (_, p)) ->
          let r =
            reject_tags p.ppat_loc "Opt"
              (aux ~left:None ~right:None ~encoding p)
          in
          no_tags (Sedlex.alt Sedlex.eps r)
      (* Compl p — complement of a character class *)
      | Ppat_construct ({ txt = Lident "Compl"; _ }, arg) -> begin
          match arg with
            | Some (_, p0) -> begin
                let r =
                  reject_tags p0.ppat_loc "Compl"
                    (aux ~left:None ~right:None ~encoding p0)
                in
                match Sedlex.compl r with
                  | Some r -> no_tags r
                  | None ->
                      err p.ppat_loc
                        "the Compl operator can only applied to a \
                         single-character length regexp"
              end
            | _ -> err p.ppat_loc "the Compl operator requires an argument"
        end
      (* Sub (a, b) — character class subtraction *)
      | Ppat_construct ({ txt = Lident "Sub"; _ }, arg) ->
          char_pair_op ~encoding Sedlex.subtract "Sub" ~loc:p.ppat_loc
            (Option.map (fun (_, arg) -> arg) arg)
      (* Intersect (a, b) — character class intersection *)
      | Ppat_construct ({ txt = Lident "Intersect"; _ }, arg) ->
          char_pair_op ~encoding Sedlex.intersection "Intersect" ~loc:p.ppat_loc
            (Option.map (fun (_, arg) -> arg) arg)
      (* Chars "..." — character set from string literal *)
      | Ppat_construct ({ txt = Lident "Chars"; _ }, arg) -> (
          let const =
            match arg with
              | Some (_, { ppat_desc = Ppat_constant const; _ }) -> Some const
              | _ -> None
          in
          match const with
            | Some (Pconst_string (s, _, _)) ->
                let l = rev_csets_of_string ~loc:p.ppat_loc ~encoding s in
                let chars = List.fold_left Cset.union Cset.empty l in
                no_tags (Sedlex.chars chars)
            | _ ->
                err p.ppat_loc "the Chars operator requires a string argument")
      (* 'a' .. 'z' or 0x41 .. 0x5a — character/codepoint range *)
      | Ppat_interval (i_start, i_end) -> begin
          match (i_start, i_end) with
            | Pconst_char c1, Pconst_char c2 ->
                let valid =
                  match encoding with
                    (* utf8 char interval can only match ascii because
                       of the OCaml lexer. *)
                    | Ascii | Utf8 -> (
                        function '\x00' .. '\x7f' -> true | _ -> false)
                    | Latin1 -> ( function _ -> true)
                in
                if not (valid c1 && valid c2) then
                  err p.ppat_loc
                    "this pattern is not a valid %s interval regexp"
                    (string_of_encoding encoding);
                no_tags
                  (Sedlex.chars (Cset.interval (Char.code c1) (Char.code c2)))
            | Pconst_integer (i1, _), Pconst_integer (i2, _) ->
                no_tags
                  (Sedlex.chars
                     (Cset.interval
                        (codepoint (int_of_string i1))
                        (codepoint (int_of_string i2))))
            | _ -> err p.ppat_loc "this pattern is not a valid interval regexp"
        end
      (* "string" or 'c' or 0x42 — literal string, char, or codepoint *)
      | Ppat_constant const -> begin
          match const with
            | Pconst_string (s, _, _) ->
                let rev_l = rev_csets_of_string s ~loc:p.ppat_loc ~encoding in
                no_tags
                  (List.fold_left
                     (fun acc cset -> Sedlex.seq (Sedlex.chars cset) acc)
                     Sedlex.eps rev_l)
            | Pconst_char c -> no_tags (Sedlex.chars (char c))
            | Pconst_integer (i, _) ->
                no_tags
                  (Sedlex.chars (Cset.singleton (codepoint (int_of_string i))))
            | _ -> err p.ppat_loc "this pattern is not a valid regexp"
        end
      (* name — reference to a previously defined regexp *)
      | Ppat_var { txt = x; _ } -> begin
          try no_tags (fst (StringMap.find x env))
          with Not_found -> err p.ppat_loc "unbound regexp %s" x
        end
      | _ -> err p.ppat_loc "this pattern is not a valid regexp"
  in
  aux ~left:(Some (Start_plus 0)) ~right:(Some (End_minus 0)) ~encoding:Ascii

(* [handle_sedlex_match_ ~env ~map_rhs match_expr] is the main entry point
   for compiling a [match%sedlex lexbuf with ...] expression. It:
   1. Extracts the lexbuf identifier and match cases.
   2. Parses each case's pattern into a regexp + tag_info via [regexp_of_pattern].
   3. Compiles all regexps into a single DFA via [Sedlex.compile].
   4. Applies [map_rhs] to each case's right-hand side (for recursive PPX
      expansion of nested [match%sedlex] blocks).
   5. Wraps each RHS with [gen_binding_code] for [as] binding extraction.
   6. Generates the full lexer code via [gen_definition].
   Returns [(generated_expr, dfa)] for use by the main mapper and tests. *)
let handle_sedlex_match_ ~env ~map_rhs match_expr =
  let lexbuf =
    match match_expr with
      | { pexp_desc = Pexp_match (lexbuf, _); _ } -> (
          match lexbuf with
            | { pexp_desc = Pexp_ident { txt = Lident txt; _ }; _ } ->
                (txt, lexbuf)
            | _ ->
                err lexbuf.pexp_loc
                  "the matched expression must be a single identifier")
      | _ ->
          err match_expr.pexp_loc
            "the %%sedlex extension is only recognized on match expressions"
  in
  let cases =
    match match_expr with
      | { pexp_desc = Pexp_match (_, cases); _ } -> cases
      | _ -> assert false
  in
  let cases = List.rev cases in
  let error =
    match List.hd cases with
      | { pc_lhs = [%pat? _]; pc_rhs = e; pc_guard = None } -> map_rhs e
      | { pc_lhs = p; _ } ->
          err p.ppat_loc "the last branch must be a catch-all error case"
  in
  let cases = List.rev (List.tl cases) in
  Sedlex.reset_tags ();
  let cases_parsed =
    List.map
      (function
        | { pc_lhs = p; pc_rhs = e; pc_guard = None } ->
            let regexp, tag_info, _anchors = regexp_of_pattern env p in
            (regexp, tag_info, e)
        | { pc_guard = Some e; _ } ->
            err e.pexp_loc "'when' guards are not supported")
      cases
  in
  let raw_compiled =
    Sedlex.compile (Array.of_list (List.map (fun (r, _, _) -> r) cases_parsed))
  in
  (* Collect live tags from all tag_infos and optimize (dead tag
     elimination + remapping to dense range). *)
  let live_tags =
    let collect_tag acc = function
      | Tag { tag; _ } -> tag :: acc
      | Start_plus _ | End_minus _ -> acc
    in
    List.fold_left
      (fun acc (_, tag_info, _) ->
        List.fold_left
          (fun acc ti ->
            let acc = collect_tag acc ti.start_pos in
            let acc = collect_tag acc ti.end_pos in
            List.fold_left (fun acc (cell, _) -> cell :: acc) acc ti.disc)
          acc tag_info)
      [] cases_parsed
  in
  let compiled, tag_mapping = Sedlex.optimize ~live:live_tags raw_compiled in
  let remap_pos = function
    | Tag { tag; offset } -> Tag { tag = tag_mapping.(tag); offset }
    | pe -> pe
  in
  let remap_tag_info ti =
    {
      ti with
      start_pos = remap_pos ti.start_pos;
      end_pos = remap_pos ti.end_pos;
      disc = List.map (fun (cell, v) -> (tag_mapping.(cell), v)) ti.disc;
    }
  in
  (* map_rhs is called after compile so that nested match%sedlex blocks
     (which call reset_tags) cannot corrupt the outer tag counter. *)
  let cases =
    List.map
      (fun (_, tag_info, e) ->
        let tag_info = List.map remap_tag_info tag_info in
        let action = gen_binding_code (snd lexbuf) tag_info (map_rhs e) in
        ((), action))
      cases_parsed
  in
  (gen_definition lexbuf compiled cases error, compiled.dfa)

let handle_sedlex_match match_expr =
  handle_sedlex_match_ ~env:builtin_regexps ~map_rhs:Fun.id match_expr

let previous = ref []
let regexps = ref []
let should_set_cookies = ref false

(* The ppxlib AST mapper. It carries an [env] of named regexps (built-in
   Unicode categories + user [let%sedlex.regexp] definitions). The [toplevel]
   flag distinguishes the outermost structure (where partition/table
   definitions are emitted) from nested modules. *)
let mapper =
  object (this)
    inherit Ast_traverse.map as super
    val env = builtin_regexps
    method define_regexp name r_len = {<env = StringMap.add name r_len env>}

    method eval_regexp_expr e =
      match e with
        (* [%sedlex.regexp? <pattern>] *)
        | [%expr [%sedlex.regexp? [%p? p]]] ->
            let r, tags, _ = regexp_of_pattern env p in
            if tags <> [] then
              err p.ppat_loc
                "'as' bindings are not allowed in regexp definitions";
            let len = fixed_length env ~encoding:Ascii p in
            Some (r, len)
        (* let <name> = [%sedlex.regexp? <pattern>] in <body> *)
        | [%expr
            let [%p? { ppat_desc = Ppat_var { txt = name; _ }; _ }] =
              [%sedlex.regexp? [%p? p]]
            in
            [%e? body]] ->
            let r, tags, _ = regexp_of_pattern env p in
            if tags <> [] then
              err p.ppat_loc
                "'as' bindings are not allowed in regexp definitions";
            let len = fixed_length env ~encoding:Ascii p in
            (this#define_regexp name (r, len))#eval_regexp_expr body
        | _ -> None

    method! expression e =
      match e with
        (* match%sedlex <lexbuf> with ... *)
        | [%expr [%sedlex [%e? { pexp_desc = Pexp_match _; _ } as match_expr]]]
          ->
            fst (handle_sedlex_match_ ~env ~map_rhs:this#expression match_expr)
        (* let <name> = <rhs> in <body>  —  intercept when <rhs> is a regexp *)
        | [%expr
            let [%p? { ppat_desc = Ppat_var { txt = name; _ }; _ }] =
              [%e? rhs]
            in
            [%e? _body]] -> (
            match this#eval_regexp_expr rhs with
              | Some r -> (this#define_regexp name r)#expression _body
              | None -> super#expression e)
        (* [%sedlex <not-a-match>]  —  error *)
        | [%expr [%sedlex [%e? _]]] ->
            err e.pexp_loc
              "the %%sedlex extension is only recognized on match expressions"
        | _ -> super#expression e

    val toplevel = true

    method structure_with_regexps l =
      let mapper = ref this in
      let regexps = ref [] in
      let l =
        List.concat
          (List.map
             (function
               (* let <name> = <e>  —  intercept top-level regexp definitions *)
               | [%stri
                   let [%p? { ppat_desc = Ppat_var { txt = name; _ }; _ }] =
                     [%e? e]] as i -> (
                   match !mapper#eval_regexp_expr e with
                     | Some r ->
                         regexps := i :: !regexps;
                         mapper := !mapper#define_regexp name r;
                         []
                     | None -> [!mapper#structure_item i])
               | i -> [!mapper#structure_item i])
             l)
      in
      (l, List.rev !regexps)

    method! structure l =
      if toplevel then (
        let sub = {<toplevel = false>} in
        let l, regexps' = sub#structure_with_regexps (!previous @ l) in
        let parts = List.map partition (get_partitions ()) in
        let tables = List.map table (get_tables ()) in
        regexps := regexps';
        should_set_cookies := true;
        tables @ parts @ l)
      else fst (this#structure_with_regexps l)
  end

let map_expression expr = mapper#expression expr

(* ppxlib cookie handlers: regexp definitions survive across compilation
   units by round-tripping through a ppxlib cookie named "sedlex.regexps". *)
let pre_handler cookies =
  previous :=
    match Driver.Cookies.get cookies "sedlex.regexps" Ast_pattern.__ with
      | Some { pexp_desc = Pexp_extension (_, PStr l); _ } -> l
      | Some _ -> assert false
      | None -> []

let post_handler cookies =
  if !should_set_cookies then (
    let loc = default_loc in
    Driver.Cookies.set cookies "sedlex.regexps"
      (pexp_extension ~loc ({ loc; txt = "regexps" }, PStr !regexps)))

(* We register via ~impl:mapper#structure rather than ~extensions so that
   partition and table definitions can be inserted at the top level. *)
(* let extensions =
     [
       Extension.declare "sedlex" Extension.Context.expression
         Ast_pattern.(single_expr_payload __)
         (fun ~loc:_ ~path:_ expr -> mapper#expression expr);
     ] *)

let () =
  Driver.Cookies.add_handler pre_handler;
  Driver.Cookies.add_post_handler post_handler;
  Driver.register_transformation "sedlex" ~impl:mapper#structure
