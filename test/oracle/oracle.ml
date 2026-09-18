(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2026, Hugo Heuzard                                           *)

(** Test oracle for the tagged-DFA compiler.

    Two independent implementations of sedlex's matching semantics are run on
    the same (rules, input) pairs and their results compared:

    - {!ref_match}: a brute-force backtracking matcher over {!Ir.t}. It
      enumerates parses in leftmost-greedy priority order (alternation prefers
      the left branch, repetition prefers more iterations) and keeps the
      highest-priority parse among those of maximal length. This is the
      specification of capture semantics.

    - {!dfa_match}: an interpreter for the output of {!Sedlex.compile_ir} that
      faithfully replicates the runtime behavior of PPX-generated code:
      mark/backtrack with memory-cell snapshots, tag operations on transitions,
      and binding extraction via discriminator cells.

    {!oracle} prints one line per input, prefixed with ERROR when the two
    disagree. {!qcheck} runs a deterministic random sweep and prints failing
    cases through {!oracle}, so an empty expect block means full agreement. *)

open Sedlex_compiler

(* ------------------------------------------------------------------ *)
(* Ir combinators                                                      *)
(* ------------------------------------------------------------------ *)

let ok = function Ok x -> x | Error e -> failwith e
let lit c = Ir.chars (Cset.singleton (Char.code c))
let cls lo hi = Ir.chars (Cset.interval (Char.code lo) (Char.code hi))
let eof = Ir.chars Cset.eof
let any = Ir.chars Cset.any
let seq r1 r2 = ok (Ir.seq r1 r2)
let ( ^. ) = seq
let alt r1 r2 = ok (Ir.alt r1 r2)
let star r = ok (Ir.star r)
let plus r = ok (Ir.plus r)
let rep r n m = ok (Ir.rep r n m)
let opt r = rep r 0 1
let capture name r = ok (Ir.capture name r)

let cset_of = function
  | Ir.Chars c -> c
  | _ -> failwith "single-character regexp expected"

let compl r = Ir.chars (Cset.difference Cset.any (cset_of r))
let sub r1 r2 = Ir.chars (Cset.difference (cset_of r1) (cset_of r2))
let inter r1 r2 = Ir.chars (Cset.intersection (cset_of r1) (cset_of r2))

(* ------------------------------------------------------------------ *)
(* Common result shape                                                 *)
(* ------------------------------------------------------------------ *)

type match_result = {
  rule : int;
  len : int;
  bindings : (string * (int option * int option)) list;
      (** Sorted by name. [None] means the position comes from a memory cell
          that was never written (only the DFA side can produce this). *)
}

(* ------------------------------------------------------------------ *)
(* Reference matcher                                                   *)
(* ------------------------------------------------------------------ *)

(* [parses ir input] lazily enumerates the (end position, capture environment)
   of every parse of [ir] starting at position 0, in leftmost-greedy priority
   order. Captures cannot occur under repetition (enforced by the Ir smart
   constructors), so repetition bodies never extend the environment and
   iterations that consume nothing can be cut without losing bindings. *)
let parses (ir : Ir.t) (input : int array) :
    (int * (string * (int * int)) list) Seq.t =
  let len = Array.length input in
  let rec go ir pos env =
    match ir with
      | Ir.Chars cs ->
          (* A real character consumes one position; the eof pseudo-character
             (-1) matches only at end of input and consumes nothing, mirroring
             the runtime where [next] reports EOF without advancing [pos]. *)
          let real =
            if pos < len && Cset.mem input.(pos) cs then Seq.return (pos + 1, env)
            else Seq.empty
          in
          let eof =
            if pos = len && Cset.mem (-1) cs then Seq.return (pos, env)
            else Seq.empty
          in
          Seq.append real eof
      | Ir.Eps -> Seq.return (pos, env)
      | Ir.Seq elems ->
          List.fold_left
            (fun acc elem -> Seq.concat_map (fun (p, e) -> go elem p e) acc)
            (Seq.return (pos, env))
            elems
      | Ir.Alt branches ->
          Seq.concat_map (fun b -> go b pos env) (List.to_seq branches)
      | Ir.Star r -> iter r pos env
      | Ir.Plus r -> Seq.concat_map (fun (p, e) -> iter r p e) (go r pos env)
      | Ir.Rep (r, n, m) -> bounded r n m pos env
      | Ir.Capture (name, r) ->
          Seq.map (fun (p, e) -> (p, (name, (pos, p)) :: e)) (go r pos env)
  and iter r pos env =
    (* Greedy: continuing the loop has priority over exiting. *)
    Seq.append
      (Seq.concat_map
         (fun (p, e) -> if p = pos then Seq.empty else iter r p e)
         (go r pos env))
      (Seq.return (pos, env))
  and bounded r n m pos env =
    if m = 0 then Seq.return (pos, env)
    else if n > 0 then
      Seq.concat_map
        (fun (p, e) -> bounded r (n - 1) (m - 1) p e)
        (go r pos env)
    else
      Seq.append
        (Seq.concat_map
           (fun (p, e) ->
             if p = pos then Seq.empty else bounded r 0 (m - 1) p e)
           (go r pos env))
        (Seq.return (pos, env))
  in
  go ir 0 []

(* Highest-priority parse among those of maximal length: enumeration order is
   priority order, so the first parse reaching the maximum wins ties. *)
let best_parse ir input =
  Seq.fold_left
    (fun best (p, e) ->
      match best with Some (bp, _) when bp >= p -> best | _ -> Some (p, e))
    None (parses ir input)

let ref_match (rules : Ir.t array) (input : int array) : match_result option =
  let best = ref None in
  Array.iteri
    (fun i ir ->
      match best_parse ir input with
        | None -> ()
        | Some (p, e) -> (
            (* Longest match wins; the lowest-numbered rule wins ties. *)
              match !best with
              | Some (_, bp, _) when bp >= p -> ()
              | _ -> best := Some (i, p, e)))
    rules;
  Option.map
    (fun (i, p, e) ->
      let bindings =
        List.sort compare
          (List.map (fun (n, (s, ep)) -> (n, (Some s, Some ep))) e)
      in
      { rule = i; len = p; bindings })
    !best

(* ------------------------------------------------------------------ *)
(* DFA interpreter                                                     *)
(* ------------------------------------------------------------------ *)

(* Replicates the runtime behavior of PPX-generated code (see [gen_state] and
   [gen_definition] in ppx_sedlex.ml):
   - memory cells start at -1 ([__private__init_mem]);
   - [init_tags] run before entering state 0, recording position 0;
   - on entering an accepting state, the rule, position and a snapshot of the
     memory cells are saved ([Sedlexing.mark]);
   - a transition's tag operations run after the code point is consumed, so
     [Set_position] records the position just past that code point;
   - a mark is kept only when it improves on the current one — strictly longer,
     or equal length with a lower rule number ([Sedlexing.mark]);
   - on a dead end, the last kept snapshot wins ([Sedlexing.backtrack]). *)

let eval_pos mem ~len (pe : Sedlex.pos_expr) =
  match pe with
    | Sedlex.Tag { tag; offset } ->
        let v = mem.(tag) in
        if v < 0 then None else Some (v + offset)
    | Sedlex.Start_plus n -> Some n
    | Sedlex.End_minus n -> Some (len - n)

(* Mirrors [gen_binding_code]: bindings are grouped by name; for a name with
   several (start, end, disc) alternatives the generated code emits an if/else
   chain on the discriminator cells, falling through to the last alternative
   unconditionally. *)
let extract_bindings (bindings : Sedlex.compiled_binding list) mem ~len =
  let names =
    List.rev
      (List.fold_left
         (fun acc (b : Sedlex.compiled_binding) ->
           if List.mem b.name acc then acc else b.name :: acc)
         [] bindings)
  in
  List.sort compare
    (List.map
       (fun name ->
         let entries =
           List.filter
             (fun (b : Sedlex.compiled_binding) -> b.name = name)
             bindings
         in
         let rec select = function
           | [] -> assert false
           | [(b : Sedlex.compiled_binding)] -> b
           | (b : Sedlex.compiled_binding) :: rest ->
               if List.for_all (fun (cell, v) -> mem.(cell) = v) b.disc then b
               else select rest
         in
         let b = select entries in
         (name, (eval_pos mem ~len b.start_pos, eval_pos mem ~len b.end_pos)))
       names)

let dfa_match (compiled : Sedlex.compiled_ir) (input : int array) :
    match_result option =
  let len = Array.length input in
  let mem = Array.make (max compiled.num_tags 0) (-1) in
  let apply pos ops =
    (* Parallel-move semantics: Copy reads observe the state before any
       write of the same operation list. *)
    let saved =
      List.filter_map
        (fun (op : Sedlex.tag_op) ->
          match op with Copy { src; _ } -> Some (src, mem.(src)) | _ -> None)
        ops
    in
    List.iter
      (fun (op : Sedlex.tag_op) ->
        match op with
          | Set_position { dst } -> mem.(dst) <- pos
          | Set_value { dst; value } -> mem.(dst) <- value
          | Copy { dst; src } -> mem.(dst) <- List.assoc src saved)
      ops
  in
  apply 0 compiled.init_tags;
  let marked = ref None in
  let transition (st : Sedlex.dfa_state) ch =
    Array.fold_left
      (fun acc (cs, tgt, ops) ->
        match acc with
          | Some _ -> acc
          | None -> if Cset.mem ch cs then Some (tgt, ops) else None)
      None st.trans
  in
  (* [eofed] records whether the EOF pseudo-character has already been fed.
     The runtime reports EOF (-1) without advancing [pos], so the eof edge is
     zero-width; the flag stops us from re-feeding it forever on an eof
     self-loop. *)
  let rec loop state pos eofed =
    let st = compiled.dfa.(state) in
    (match st.accept with
      | Some (r, ops) ->
          (* Materialize registers into canonical cells (final_ops always run),
             then keep the mark only if it improves on the current one — longer,
             or equal length with a lower rule number. *)
          apply pos ops;
          let improves =
            match !marked with
              | None -> true
              | Some (mr, mp, _) -> pos > mp || (pos = mp && r < mr)
          in
          if improves then marked := Some (r, pos, Array.copy mem)
      | None -> ());
    if pos < len then (
      match transition st input.(pos) with
        | None -> ()
        | Some (tgt, ops) ->
            apply (pos + 1) ops;
            loop tgt (pos + 1) eofed)
    else if not eofed then (
      (* End of input: feed EOF (-1) as a zero-width step. *)
      match transition st (-1) with
        | None -> ()
        | Some (tgt, ops) ->
            apply pos ops;
            loop tgt pos true)
  in
  loop 0 0 false;
  Option.map
    (fun (r, p, m) ->
      {
        rule = r;
        len = p;
        bindings = extract_bindings compiled.bindings.(r) m ~len:p;
      })
    !marked

(* ------------------------------------------------------------------ *)
(* Comparison and printing                                             *)
(* ------------------------------------------------------------------ *)

let show_result input (r : match_result option) =
  match r with
    | None -> "no match"
    | Some { rule; len; bindings } ->
        let buf = Buffer.create 32 in
        Printf.bprintf buf "rule %d, len %d" rule len;
        if bindings <> [] then (
          Buffer.add_string buf ", [";
          List.iteri
            (fun i (name, (s, e)) ->
              if i > 0 then Buffer.add_string buf ", ";
              match (s, e) with
                | Some s, Some e
                  when 0 <= s && s <= e && e <= Array.length input ->
                    Printf.bprintf buf "%s=%S" name
                      (String.init (e - s) (fun i ->
                           let c = input.(s + i) in
                           if c >= 32 && c < 127 then Char.chr c else '?'))
                | s, e ->
                    let p = function
                      | None -> "unset"
                      | Some n -> string_of_int n
                    in
                    Printf.bprintf buf "%s=<%s..%s>" name (p s) (p e))
            bindings;
          Buffer.add_string buf "]");
        Buffer.contents buf

let codes s = Array.init (String.length s) (fun i -> Char.code s.[i])

let compile rules =
  try Ok (Sedlex.compile_ir rules) with exn -> Error (Printexc.to_string exn)

let oracle rules input_str =
  let input = codes input_str in
  let ref_s = show_result input (ref_match rules input) in
  match compile rules with
    | Error e -> Printf.printf "ERROR %S -> compile_ir raised: %s\n" input_str e
    | Ok compiled ->
        let dfa_s = show_result input (dfa_match compiled input) in
        if String.equal ref_s dfa_s then
          Printf.printf "%S -> %s\n" input_str ref_s
        else Printf.printf "ERROR %S -> ref=%s dfa=%s\n" input_str ref_s dfa_s

let check rules input_str =
  let input = codes input_str in
  match compile rules with
    | Error _ -> false
    | Ok compiled ->
        String.equal
          (show_result input (ref_match rules input))
          (show_result input (dfa_match compiled input))

(* ------------------------------------------------------------------ *)
(* Random pattern generation                                           *)
(* ------------------------------------------------------------------ *)

module G = QCheck2.Gen

let gen_char = G.char_range 'a' 'd'

let gen_cset =
  G.oneof
    [
      G.map (fun c -> Cset.singleton (Char.code c)) gen_char;
      G.map2
        (fun a b ->
          let a = Char.code a and b = Char.code b in
          Cset.interval (min a b) (max a b))
        gen_char gen_char;
    ]

(* A terminal end-of-input anchor: nothing, a bare [eof], or a data-dependent
   [c | eof] cset. Placed only at the end of a rule, where [eof] realistically
   appears; this exercises the zero-width eof paths (offset math across eof,
   mixed-width csets) without generating degenerate eof-in-the-middle shapes. *)
let gen_eof_anchor =
  G.oneof_weighted
    [
      (3, G.pure None);
      (1, G.pure (Some (Ir.chars Cset.eof)));
      ( 1,
        G.map
          (fun c ->
            Some (Ir.chars (Cset.union (Cset.singleton (Char.code c)) Cset.eof)))
          gen_char );
    ]

(* Capture-free regexp of bounded depth. *)
let rec gen_simple depth =
  if depth = 0 then G.map Ir.chars gen_cset
  else (
    let sub = gen_simple (depth - 1) in
    G.oneof_weighted
      [
        (3, G.map Ir.chars gen_cset);
        (2, G.map2 seq sub sub);
        (2, G.map2 alt sub sub);
        (1, G.map star sub);
        (1, G.map plus sub);
        ( 1,
          G.map2
            (fun r (n, m) -> rep r n m)
            sub
            (G.map2 (fun n k -> (n, n + k)) (G.int_range 0 2) (G.int_range 0 2))
        );
      ])

let names = [| "x"; "y"; "z"; "w" |]

(* A rule: a sequence of up to 4 elements with at least one capture. Captures
   sit at the top level of the sequence (the Ir smart constructors reject them
   under repetition), each binding a distinct name. *)
let gen_rule ?(eof = false) () =
  G.bind (G.int_range 1 4) (fun n ->
      G.bind
        (G.int_range 0 (n - 1))
        (fun cap_at ->
          let gen_elem i =
            let simple = G.bind (G.int_range 0 2) gen_simple in
            if i = cap_at then G.map (capture names.(i)) simple
            else
              G.oneof_weighted
                [(3, simple); (1, G.map (capture names.(i)) simple)]
          in
          let rec build i acc =
            if i = n then acc else build (i + 1) (G.map2 seq acc (gen_elem i))
          in
          let body = build 1 (gen_elem 0) in
          if not eof then body
          else
            G.map2
              (fun body anchor ->
                match anchor with Some a -> seq body a | None -> body)
              body gen_eof_anchor))

(* An or-pattern rule: both branches bind the same name (discriminators). *)
let gen_or_rule =
  let branch = G.bind (G.int_range 0 2) gen_simple in
  G.map2 (fun a b -> alt (capture "x" a) (capture "x" b)) branch branch

(* The terminal [eof] anchor gives regression coverage for both the
   fixed_length eof-width bug and the eof/rule-priority tie (an earlier rule and
   a later [eof]-terminated rule matching the same length: the earlier rule must
   win). *)
let gen_ir ?eof () = G.oneof_weighted [(4, gen_rule ?eof ()); (1, gen_or_rule)]
let gen_input = G.string_size ~gen:gen_char (G.int_range 0 5)

(* Deterministic property runner: fixed seed, no shrinking. Failing cases are
   printed through [oracle] so expect blocks capture them; an empty expect
   block means reference and DFA agree on every generated case. *)
let qcheck ?(count = 2000) ?(max_print = 5) gen =
  let rand = Random.State.make [| 0x5ed1ec5 |] in
  let failures = ref 0 in
  for _ = 1 to count do
    let rules, input = G.generate1 ~rand gen in
    if not (check rules input) then (
      incr failures;
      if !failures <= max_print then (
        Array.iteri
          (fun i ir -> Printf.printf "  rule%d: %s\n" i (Ir.show ir))
          rules;
        oracle rules input))
  done;
  if !failures > 0 then
    Printf.printf "%d/%d cases failed (printed at most %d)\n" !failures count
      max_print
