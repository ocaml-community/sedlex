(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2026, Hugo Heuzard                                           *)

(** ocamllex as a second reference for capture semantics.

    A rule set over {!Ir.t} is translated to ocamllex's syntax, compiled with
    the vendored [Lexgen.make_dfa], and the resulting automaton is run on the
    input. The result is compared with the reference matcher of {!Oracle}.

    End of input is not translated: ocamllex can read it repeatedly, sedlex
    reads it once, so the random sweep keeps it out of the patterns. *)

open Sedlex_compiler
open Ocamllex_vendored
module Oracle = Oracle_test.Oracle

let dummy_loc : Syntax.location =
  { loc_file = ""; start_pos = 0; end_pos = 0; start_line = 0; start_col = 0 }

(* The sweep alphabet is a..d, so clamping to bytes loses nothing. *)
let convert_cset (cset : Sedlex_compiler.Cset.t) : Cset.t =
  List.fold_left
    (fun acc (lo, hi) ->
      if lo < 0 then failwith "eof is not translated"
      else if lo > 255 then acc
      else Cset.union acc (Cset.interval lo (min hi 255)))
    Cset.empty
    (Sedlex_compiler.Cset.to_list cset)

(* Alternatives are ordered left to right and repetition is written so that
   one more iteration comes first, like sedlex's leftmost-greedy order. *)
let rec convert (ir : Ir.t) : Syntax.regular_expression =
  match ir with
    | Chars cset -> Characters (convert_cset cset)
    | Eps -> Epsilon
    | Seq elems ->
        List.fold_left
          (fun acc e -> Syntax.Sequence (acc, convert e))
          Epsilon elems
    | Alt [] -> Epsilon
    | Alt (first :: rest) ->
        List.fold_left
          (fun acc b -> Syntax.Alternative (acc, convert b))
          (convert first) rest
    | Star inner -> Repetition (convert inner)
    | Plus inner ->
        let r = convert inner in
        Sequence (r, Repetition r)
    | Rep (inner, lo, hi) ->
        let r = convert inner in
        let rec mandatory n acc =
          if n <= 0 then acc else mandatory (n - 1) (Syntax.Sequence (r, acc))
        in
        let rec optional n acc =
          if n <= 0 then acc
          else
            optional (n - 1) (Syntax.Alternative (Sequence (r, acc), Epsilon))
        in
        mandatory lo (optional (hi - lo) Epsilon)
    | Capture (name, inner) -> Bind (convert inner, (name, dummy_loc))

let entry (rules : Ir.t array) : (string list, int) Syntax.entry =
  {
    name = "oracle";
    shortest = false;
    args = [];
    clauses = Array.to_list (Array.mapi (fun i ir -> (convert ir, i)) rules);
  }

(* Runs the automaton the way ocamllex's engine does: memory actions on the
   move, tag actions when a final state is remembered, the last remembered
   action wins on backtrack. Bindings are resolved from the action's
   environment against the memory as it was when the action was remembered. *)
let simulate (rules : Ir.t array) (input : int array) :
    Oracle.match_result option =
  let entries, auto = Lexgen.make_dfa [entry rules] in
  let entry = List.hd entries in
  let mem = Array.make entry.auto_mem_size (-1) in
  let memory_actions pos actions =
    List.iter
      (fun (a : Lexgen.memory_action) ->
        match a with
          | Set dst -> mem.(dst) <- pos
          | Copy (dst, src) -> mem.(dst) <- mem.(src))
      actions
  in
  let tag_actions actions =
    List.iter
      (fun (a : Lexgen.tag_action) ->
        match a with
          | SetTag (dst, src) -> mem.(dst) <- mem.(src)
          | EraseTag dst -> mem.(dst) <- -1)
      actions
  in
  let init_state, init_moves = entry.auto_initial_state in
  memory_actions 0 init_moves;
  let marked = ref None in
  let remember action tags pos =
    tag_actions tags;
    marked := Some (action, pos, Array.copy mem)
  in
  let rec loop state pos =
    match auto.(state) with
      | Lexgen.Perform (action, tags) -> remember action tags pos
      | Lexgen.Shift (trans, moves) -> (
          (match trans with
            | Remember (action, tags) -> remember action tags pos
            | No_remember -> ());
          let byte = if pos < Array.length input then input.(pos) else 256 in
          let move, actions = moves.(byte) in
          memory_actions (pos + 1) actions;
          match move with
            | Goto target -> loop target (pos + 1)
            | Backtrack -> ())
  in
  loop init_state 0;
  Option.map
    (fun (action, len, mem) ->
      let _, env, _ =
        List.find (fun (n, _, _) -> n = action) entry.auto_actions
      in
      let addr (Lexgen.Sum (base, offset)) =
        let base =
          match base with
            | Lexgen.Start -> 0
            | Lexgen.End -> len
            | Lexgen.Mem n -> mem.(n)
        in
        if base < 0 then None else Some (base + offset)
      in
      let bindings =
        List.sort compare
          (List.map
             (fun ((name, _), (info : Lexgen.ident_info)) ->
               match info with
                 | Ident_string (_, s, e) -> (name, (addr s, addr e))
                 | Ident_char (_, p) ->
                     (name, (addr p, Option.map succ (addr p))))
             env)
      in
      { Oracle.rule = action; len; bindings })
    !marked

(* [other_parse rules input lex] holds when [lex] is the reference's rule and
   length with the bindings of another parse of maximal length: a different
   disambiguation of the same ambiguity, not an error. ocamllex gives the
   earlier sub-pattern the longest match where sedlex prefers the left
   alternative. *)
let other_parse rules input (lex : Oracle.match_result) =
  let len = Array.length input in
  let same_bindings env =
    let b =
      List.sort compare
        (List.map
           (fun (n, (s, e)) -> (n, (Some (min s len), Some (min e len))))
           env)
    in
    b = lex.bindings
  in
  Seq.exists
    (fun (p, env) -> min p len = lex.len && same_bindings env)
    (Oracle.parses rules.(lex.rule) input)

type verdict = Agree | Other_parse | Disagree

let compare_results rules input =
  let ref_r = Oracle.ref_match rules input in
  let lex_r = simulate rules input in
  let verdict =
    match (ref_r, lex_r) with
      | Some r, Some l when r.rule = l.rule && r.len = l.len ->
          if r.bindings = l.bindings then Agree
          else if other_parse rules input l then Other_parse
          else Disagree
      | None, None -> Agree
      | _ -> Disagree
  in
  (verdict, ref_r, lex_r)

(* Reference matcher vs ocamllex. A line reads [ERROR] when ocamllex's result
   is not a parse the reference admits, and shows ocamllex's bindings after
   the reference's when they are another disambiguation. *)
let oracle rules input_str =
  let input = Oracle.codes input_str in
  let verdict, ref_r, lex_r = compare_results rules input in
  let ref_s = Oracle.show_result input ref_r in
  let lex_s = Oracle.show_result input lex_r in
  match verdict with
    | Agree -> Printf.printf "%S -> %s\n" input_str ref_s
    | Other_parse ->
        Printf.printf "%S -> %s (ocamllex: %s)\n" input_str ref_s lex_s
    | Disagree ->
        Printf.printf "ERROR %S -> ref=%s ocamllex=%s\n" input_str ref_s lex_s

let check rules input_str =
  let verdict, _, _ = compare_results rules (Oracle.codes input_str) in
  verdict <> Disagree

let qcheck ?count ?max_print gen =
  Oracle.sweep ?count ?max_print ~check ~oracle gen
