(* Oracle using vendored ocamllex DFA compiler.

   Converts Ir.t patterns to ocamllex's Syntax.regular_expression, compiles
   them with Lexgen.make_dfa, then simulates the resulting automata to produce
   match results comparable to the sedlex oracle. *)

open Ocamllex_vendored
module Sedlex_cset = Sedlex_compiler.Cset

(* ================================================================== *)
(* Ir.t → Syntax.regular_expression conversion                       *)
(* ================================================================== *)

let dummy_loc : Syntax.location =
  { loc_file = ""; start_pos = 0; end_pos = 0; start_line = 0; start_col = 0 }

(* Convert sedlex Cset.t (Unicode intervals) to ocamllex Cset.t (byte intervals).
   Clamps intervals to the byte range 0-255; intervals entirely above 255 are dropped. *)
let convert_cset (cset : Sedlex_cset.t) : Cset.t =
  List.fold_left
    (fun acc (lo, hi) ->
      if lo > 255 then acc else Cset.union acc (Cset.interval lo (min hi 255)))
    Cset.empty (Sedlex_cset.to_list cset)

open Sedlex_compiler.Ir

let rec convert (ir : Sedlex_compiler.Ir.t) : Syntax.regular_expression =
  match ir with
    | Chars cset -> Characters (convert_cset cset)
    | Eps -> Epsilon
    | Seq elems ->
        List.fold_left
          (fun acc e -> Syntax.Sequence (acc, convert e))
          Epsilon elems
    | Alt branches -> (
        match branches with
          | [] -> Epsilon
          | [b] -> convert b
          | first :: rest ->
              List.fold_left
                (fun acc b -> Syntax.Alternative (acc, convert b))
                (convert first) rest)
    | Star inner -> Repetition (convert inner)
    | Plus inner -> Sequence (convert inner, Repetition (convert inner))
    | Rep (inner, lo, hi) ->
        let r = convert inner in
        let mandatory =
          let rec repeat n acc =
            if n <= 0 then acc else repeat (n - 1) (Syntax.Sequence (r, acc))
          in
          repeat lo Epsilon
        in
        let optional =
          let rec opt_repeat n acc =
            if n <= 0 then acc
            else
              opt_repeat (n - 1)
                (Syntax.Alternative (Epsilon, Sequence (r, acc)))
          in
          opt_repeat (hi - lo) Epsilon
        in
        Sequence (mandatory, optional)
    | Capture (name, inner) -> Bind (convert inner, (name, dummy_loc))

(* ================================================================== *)
(* Build ocamllex entry from Ir.t rules                               *)
(* ================================================================== *)

let build_entry (rules : Sedlex_compiler.Ir.t array) :
    (string list, int) Syntax.entry =
  let clauses =
    Array.to_list (Array.mapi (fun i ir -> (convert ir, i)) rules)
  in
  { name = "oracle"; shortest = false; args = []; clauses }

(* ================================================================== *)
(* Automata simulation                                                *)
(* ================================================================== *)

type binding = { name : string; start_offset : int; end_offset : int }
type result = { rule : int; length : int; bindings : binding list }

let simulate_automata (entries : (string list, int) Lexgen.automata_entry list)
    (auto : Lexgen.automata array) (input : int array) : result option =
  let entry = List.hd entries in
  let mem = Array.make entry.auto_mem_size (-1) in
  let init_state, init_moves = entry.auto_initial_state in
  (* Execute initial memory actions *)
  let exec_mem_actions pos actions =
    List.iter
      (fun (action : Lexgen.memory_action) ->
        match action with
          | Set dst -> mem.(dst) <- pos
          | Copy (dst, src) -> mem.(dst) <- mem.(src))
      actions
  in
  let exec_tag_actions actions =
    List.iter
      (fun (action : Lexgen.tag_action) ->
        match action with
          | SetTag (dst, src) -> mem.(dst) <- mem.(src)
          | EraseTag dst -> mem.(dst) <- -1)
      actions
  in
  exec_mem_actions 0 init_moves;
  let last_action = ref (-1) in
  let last_pos = ref 0 in
  let last_mem = ref (Array.copy mem) in
  let pos = ref 0 in
  let state = ref init_state in
  let running = ref true in
  while !running do
    match auto.(!state) with
      | Lexgen.Perform (action, tag_ops) ->
          exec_tag_actions tag_ops;
          last_action := action;
          last_pos := !pos;
          last_mem := Array.copy mem;
          running := false
      | Lexgen.Shift (remember, moves) -> (
          (match remember with
            | Remember (action, tag_ops) ->
                exec_tag_actions tag_ops;
                last_action := action;
                last_pos := !pos;
                last_mem := Array.copy mem
            | No_remember -> ());
          let byte =
            if !pos >= Array.length input then 256 (* eof *) else input.(!pos)
          in
          let move, mem_actions = moves.(byte) in
          exec_mem_actions (!pos + 1) mem_actions;
          match move with
            | Goto target ->
                incr pos;
                state := target
            | Backtrack -> running := false)
  done;
  if !last_action < 0 then None
  else (
    (* Resolve bindings from the action's t_env *)
    let action_num = !last_action in
    let match_len = !last_pos in
    let saved_mem = !last_mem in
    let env =
      try
        let _, env, _ =
          List.find (fun (n, _, _) -> n = action_num) entry.auto_actions
        in
        env
      with Not_found -> []
    in
    let resolve_addr (Lexgen.Sum (base, offset)) =
      (match base with
        | Lexgen.Start -> 0
        | Lexgen.End -> match_len
        | Lexgen.Mem n -> saved_mem.(n))
      + offset
    in
    let bindings =
      List.filter_map
        (fun ((name, _loc), (info : Lexgen.ident_info)) ->
          match info with
            | Ident_string (is_opt, start_addr, end_addr) ->
                let s = resolve_addr start_addr in
                let e = resolve_addr end_addr in
                if is_opt && s < 0 then None
                else Some { name; start_offset = s; end_offset = e }
            | Ident_char (is_opt, addr) ->
                let p = resolve_addr addr in
                if is_opt && p < 0 then None
                else Some { name; start_offset = p; end_offset = p + 1 })
        env
      |> List.sort compare
    in
    Some { rule = action_num; length = match_len; bindings })

(* ================================================================== *)
(* Public API                                                         *)
(* ================================================================== *)

let simulate (rules : Sedlex_compiler.Ir.t array) (input : int array) :
    result option =
  let entry = build_entry rules in
  let entries, auto = Lexgen.make_dfa [entry] in
  simulate_automata entries auto input
