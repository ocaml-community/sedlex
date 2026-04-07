(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

open Ppxlib
open Ast_builder.Default
open Ast_helper
open Sedlex_compiler

(* let ocaml_version = Versions.ocaml_408 *)

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
        (c : Cset.t :> (int * int) list))
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
    (fun acc (n, c) -> StringMap.add n (Ir.chars c) acc)
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

(* Code generation for `as` bindings.

   [gen_binding_code] turns the [compiled_binding list] (from the compiler)
   into [let] bindings that extract sub-matches from the lexbuf's memory cells
   (or computed offsets). For or-patterns with discriminators it emits a chain
   of if/else checks on the discriminator cell to select the correct positions. *)

(* [gen_pos_expr lexbuf pe] generates code that evaluates a [pos_expr]
   to an integer position (offset from token start, in code points). *)
let gen_pos_expr lexbuf (pe : Sedlex.pos_expr) =
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

(* [gen_binding_code lexbuf bindings action] wraps [action] with [let]
   bindings that extract sub-match values from the lexbuf's memory cells.
   For a single binding (no or-pattern), emits a direct sub_lexeme call.
   For or-patterns with multiple tag pairs, emits a chain of
   [if disc_cell = value then ...] to select the correct positions at runtime. *)
let gen_binding_code lexbuf (bindings : Sedlex.compiled_binding list) action =
  let loc = default_loc in
  ignore loc;
  if bindings = [] then action
  else (
    (* Group bindings by variable name *)
    let by_name =
      let tbl = Hashtbl.create 8 in
      let order = ref [] in
      List.iter
        (fun ({ name; start_pos; end_pos; disc } : Sedlex.compiled_binding) ->
          if not (Hashtbl.mem tbl name) then order := name :: !order;
          let existing = try Hashtbl.find tbl name with Not_found -> [] in
          Hashtbl.replace tbl name (existing @ [(start_pos, end_pos, disc)]))
        bindings;
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

(* [ir_of_pattern env] returns a function [pattern -> Ir.t] that parses an
   OCaml pattern AST into an IR node. [env] maps names to previously defined
   IR patterns. All tag allocation, discriminator handling, and fixed-length
   optimization are deferred to the compiler's [compile_ir]. *)
let ir_of_pattern env =
  let reject_captures loc ctx ir =
    if Ir.capture_names ir <> [] then
      err loc "'as' bindings are not supported inside %s" ctx;
    ir
  in
  let rec char_pair_op func name ~encoding ~loc tuple =
    (* Construct something like Sub(a,b) *)
      match tuple with
      | Some { ppat_desc = Ppat_tuple [p0; p1]; _ } -> (
          let r0 = reject_captures p0.ppat_loc name (aux ~encoding p0) in
          let r1 = reject_captures p1.ppat_loc name (aux ~encoding p1) in
          match func r0 r1 with
            | Some r -> r
            | None ->
                err loc
                  "the %s operator can only applied to single-character length \
                   regexps"
                  name)
      | _ ->
          err loc "the %s operator requires two arguments, like %s(a,b)" name
            name
  and ir_compl r =
    match r with
      | Ir.Chars c -> Some (Ir.Chars (Cset.difference Cset.any c))
      | _ -> None
  and ir_subtract r0 r1 =
    match (r0, r1) with
      | Ir.Chars c0, Ir.Chars c1 -> Some (Ir.Chars (Cset.difference c0 c1))
      | _ -> None
  and ir_intersection r0 r1 =
    match (r0, r1) with
      | Ir.Chars c0, Ir.Chars c1 -> Some (Ir.Chars (Cset.intersection c0 c1))
      | _ -> None
  and aux ~encoding p =
    match p.ppat_desc with
      (* name as x — named sub-match binding *)
      | Ppat_alias (inner, { txt = name; loc = name_loc }) ->
          let ir_inner = aux ~encoding inner in
          if List.mem name (Ir.capture_names ir_inner) then
            err name_loc
              "'as' binding '%s' shadows an inner binding of the same name" name;
          Ir.capture name ir_inner
      (* p1 | p2 — alternation *)
      | Ppat_or (p1, p2) -> Ir.alt (aux ~encoding p1) (aux ~encoding p2)
      (* (p1, p2, ...) — sequence *)
      | Ppat_tuple (p :: pl) ->
          List.fold_left
            (fun acc p -> Ir.seq acc (aux ~encoding p))
            (aux ~encoding p) pl
      (* Star p — zero-or-more repetition *)
      | Ppat_construct ({ txt = Lident "Star"; _ }, Some (_, p)) ->
          Ir.star (reject_captures p.ppat_loc "Star" (aux ~encoding p))
      (* Plus p — one-or-more repetition *)
      | Ppat_construct ({ txt = Lident "Plus"; _ }, Some (_, p)) ->
          Ir.plus (reject_captures p.ppat_loc "Plus" (aux ~encoding p))
      (* Utf8 p — switch to UTF-8 encoding *)
      | Ppat_construct ({ txt = Lident "Utf8"; _ }, Some (_, p)) ->
          aux ~encoding:Utf8 p
      (* Latin1 p — switch to Latin-1 encoding *)
      | Ppat_construct ({ txt = Lident "Latin1"; _ }, Some (_, p)) ->
          aux ~encoding:Latin1 p
      (* Ascii p — switch to ASCII encoding *)
      | Ppat_construct ({ txt = Lident "Ascii"; _ }, Some (_, p)) ->
          aux ~encoding:Ascii p
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
                } ) ) -> (
          let r = reject_captures p0.ppat_loc "Rep" (aux ~encoding p0) in
          match (i1, i2) with
            | Pconst_integer (i1, _), Pconst_integer (i2, _) ->
                let i1 = int_of_string i1 in
                let i2 = int_of_string i2 in
                if 0 <= i1 && i1 <= i2 then Ir.rep r i1 i2
                else err p.ppat_loc "Invalid range for Rep operator"
            | _ ->
                err p.ppat_loc "Rep must take an integer constant or interval")
      (* Rep _ — malformed *)
      | Ppat_construct ({ txt = Lident "Rep"; _ }, _) ->
          err p.ppat_loc "the Rep operator takes 2 arguments"
      (* Opt p — optional (zero or one) *)
      | Ppat_construct ({ txt = Lident "Opt"; _ }, Some (_, p)) ->
          let r = reject_captures p.ppat_loc "Opt" (aux ~encoding p) in
          Ir.alt Ir.eps r
      (* Compl p — complement of a character class *)
      | Ppat_construct ({ txt = Lident "Compl"; _ }, arg) -> (
          match arg with
            | Some (_, p0) -> (
                let r =
                  reject_captures p0.ppat_loc "Compl" (aux ~encoding p0)
                in
                match ir_compl r with
                  | Some r -> r
                  | None ->
                      err p.ppat_loc
                        "the Compl operator can only applied to a \
                         single-character length regexp")
            | _ -> err p.ppat_loc "the Compl operator requires an argument")
      (* Sub (a, b) — character class subtraction *)
      | Ppat_construct ({ txt = Lident "Sub"; _ }, arg) ->
          char_pair_op ~encoding ir_subtract "Sub" ~loc:p.ppat_loc
            (Option.map (fun (_, arg) -> arg) arg)
      (* Intersect (a, b) — character class intersection *)
      | Ppat_construct ({ txt = Lident "Intersect"; _ }, arg) ->
          char_pair_op ~encoding ir_intersection "Intersect" ~loc:p.ppat_loc
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
                Ir.chars chars
            | _ ->
                err p.ppat_loc "the Chars operator requires a string argument")
      (* 'a' .. 'z' or 0x41 .. 0x5a — character/codepoint range *)
      | Ppat_interval (i_start, i_end) -> (
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
                Ir.chars (Cset.interval (Char.code c1) (Char.code c2))
            | Pconst_integer (i1, _), Pconst_integer (i2, _) ->
                Ir.chars
                  (Cset.interval
                     (codepoint (int_of_string i1))
                     (codepoint (int_of_string i2)))
            | _ -> err p.ppat_loc "this pattern is not a valid interval regexp")
      (* "string" or 'c' or 0x42 — literal string, char, or codepoint *)
      | Ppat_constant const -> (
          match const with
            | Pconst_string (s, _, _) ->
                let rev_l = rev_csets_of_string s ~loc:p.ppat_loc ~encoding in
                List.fold_left
                  (fun acc cset -> Ir.seq (Ir.chars cset) acc)
                  Ir.eps rev_l
            | Pconst_char c -> Ir.chars (char c)
            | Pconst_integer (i, _) ->
                Ir.chars (Cset.singleton (codepoint (int_of_string i)))
            | _ -> err p.ppat_loc "this pattern is not a valid regexp")
      (* name — reference to a previously defined regexp *)
      | Ppat_var { txt = x; _ } -> (
          match StringMap.find_opt x env with
            | Some ir -> ir
            | None -> err p.ppat_loc "unbound regexp %s" x)
      | _ -> err p.ppat_loc "this pattern is not a valid regexp"
  in
  aux ~encoding:Ascii

(* [handle_sedlex_match_ ~env ~map_rhs match_expr] is the main entry point
   for compiling a [match%sedlex lexbuf with ...] expression. It:
   1. Extracts the lexbuf identifier and match cases.
   2. Parses each case's pattern into [Ir.t] via [ir_of_pattern].
   3. Compiles all patterns via [Sedlex.compile_ir] (tag allocation, DFA).
   4. Applies [map_rhs] to each case's right-hand side.
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
  let cases_with_ir =
    List.map
      (function
        | { pc_lhs = p; pc_rhs = e; pc_guard = None } ->
            let ir = ir_of_pattern env p in
            (match Ir.validate ir with
              | Ok () -> ()
              | Error msg -> err p.ppat_loc "%s" msg);
            (ir, p.ppat_loc, e)
        | { pc_guard = Some e; _ } ->
            err e.pexp_loc "'when' guards are not supported")
      cases
  in
  let compiled =
    Sedlex.compile_ir
      (Array.of_list (List.map (fun (ir, _, _) -> ir) cases_with_ir))
  in
  (* map_rhs is called after compile_ir so that nested match%sedlex blocks
     (which call compile_ir, resetting tags) cannot corrupt the outer
     tag counter. *)
  let cases =
    List.mapi
      (fun i (_, _, e) ->
        let action =
          gen_binding_code (snd lexbuf)
            (Array.get compiled.bindings i)
            (map_rhs e)
        in
        ((), action))
      cases_with_ir
  in
  let compiled_basic : Sedlex.compiled =
    {
      dfa = compiled.dfa;
      init_tags = compiled.init_tags;
      num_tags = compiled.num_tags;
    }
  in
  (gen_definition lexbuf compiled_basic cases error, compiled.dfa)

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
    method define_regexp name ir = {<env = StringMap.add name ir env>}

    method eval_regexp_expr e =
      match e with
        (* [%sedlex.regexp? <pattern>] *)
        | [%expr [%sedlex.regexp? [%p? p]]] ->
            let ir = ir_of_pattern env p in
            if Ir.capture_names ir <> [] then
              err p.ppat_loc
                "'as' bindings are not allowed in regexp definitions";
            Some ir
        (* let <name> = [%sedlex.regexp? <pattern>] in <body> *)
        | [%expr
            let [%p? { ppat_desc = Ppat_var { txt = name; _ }; _ }] =
              [%sedlex.regexp? [%p? p]]
            in
            [%e? body]] ->
            let ir = ir_of_pattern env p in
            if Ir.capture_names ir <> [] then
              err p.ppat_loc
                "'as' bindings are not allowed in regexp definitions";
            (this#define_regexp name ir)#eval_regexp_expr body
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
