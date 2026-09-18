(* Oracle: verify that the DFA compiled from IR preserves the language
   and that tagged transitions (for as-bindings) record correct positions.

   Two independent simulators are compared:
   - ocamllex: vendored ocamllex DFA compiler (independent TDFA implementation)
   - DFA: sedlex's own determinized tagged DFA

   ocamllex serves as the reference oracle. The DFA is the system under test. *)

open Sedlex_compiler

(* ================================================================== *)
(* Convenience builders for Ir.t                                      *)
(* ================================================================== *)

let unwrap = function Ok t -> t | Error msg -> failwith msg
let lit c = Ir.chars (Cset.singleton (Char.code c))
let cls a b = Ir.chars (Cset.interval (Char.code a) (Char.code b))
let seq a b = Ir.seq a b
let ( ^. ) = seq
let alt a b = Ir.alt a b |> unwrap
let star r = Ir.star r |> unwrap
let plus r = Ir.plus r |> unwrap
let opt r = alt Ir.eps r
let rep r lo hi = Ir.rep r lo hi |> unwrap

let compl r =
  match (r : Ir.t) with
    | Chars cset -> Ir.chars (Cset.difference Cset.any cset)
    | _ -> failwith "compl: operand must be Chars"

let sub a b =
  match ((a : Ir.t), (b : Ir.t)) with
    | Chars c1, Chars c2 -> Ir.chars (Cset.difference c1 c2)
    | _ -> failwith "sub: operands must be Chars"

let inter a b =
  match ((a : Ir.t), (b : Ir.t)) with
    | Chars c1, Chars c2 -> Ir.chars (Cset.intersection c1 c2)
    | _ -> failwith "inter: operands must be Chars"

let capture name r = Ir.capture name r |> unwrap

(* ================================================================== *)
(* DFA simulator                                                      *)
(* ================================================================== *)

type binding = {
  name : string;
  start_offset : int;  (** Start position in input, or [unset] *)
  end_offset : int;  (** End position (exclusive) in input, or [unset] *)
}

let unset = -1

type result = { rule : int; length : int; bindings : binding list }

let encode_value v = -2 - v

let exec_tag (tags : int array) pos = function
  | Sedlex.Set_position cell -> tags.(cell) <- pos
  | Sedlex.Set_value (cell, v) -> tags.(cell) <- encode_value v

let resolve_pos_expr tags length = function
  | Sedlex.Tag { tag; offset } -> tags.(tag) + offset
  | Sedlex.Start_plus n -> n
  | Sedlex.End_minus n -> length - n

let resolve_bindings tags length (cbs : Sedlex.compiled_binding list) =
  List.filter_map
    (fun (cb : Sedlex.compiled_binding) ->
      let active =
        match cb.disc with
          | [] -> true
          | conditions ->
              List.for_all
                (fun (cell, value) -> tags.(cell) = encode_value value)
                conditions
      in
      if not active then None
      else
        Some
          {
            name = cb.name;
            start_offset = resolve_pos_expr tags length cb.start_pos;
            end_offset = resolve_pos_expr tags length cb.end_pos;
          })
    cbs
  |> List.sort compare

module DFA = struct
  let find_trans (trans : (Cset.t * int * _) array) c =
    let r = ref None in
    Array.iter
      (fun (cs, tgt, ops) ->
        if !r = None && Cset.mem c cs then r := Some (tgt, ops))
      trans;
    !r

  let accepting (s : Sedlex.dfa_state) =
    let r = ref None in
    for i = 0 to Array.length s.finals - 1 do
      if !r = None && s.finals.(i) then r := Some i
    done;
    !r

  (* Simulate DFA with longest-match / first-rule-wins. *)
  let simulate_ir (compiled : Sedlex.compiled_ir) input =
    let dfa = compiled.dfa in
    let binds_per_rule = compiled.bindings in
    let tags = Array.make compiled.num_tags unset in
    List.iter (exec_tag tags 0) compiled.init_tags;
    let last = ref None in
    let si = ref 0 in
    let pos = ref 0 in
    let continue = ref true in
    while !continue do
      (match accepting dfa.(!si) with
        | Some r ->
            let bindings =
              resolve_bindings (Array.copy tags) !pos binds_per_rule.(r)
            in
            last := Some { rule = r; length = !pos; bindings }
        | None -> ());
      if !pos >= Array.length input then continue := false
      else (
        match find_trans dfa.(!si).trans input.(!pos) with
          | None -> continue := false
          | Some (tgt, ops) ->
              List.iter (exec_tag tags (!pos + 1)) ops;
              si := tgt;
              incr pos)
    done;
    !last
end

(* ================================================================== *)
(* Oracle: run ocamllex + DFA, compare, extract + verify captures     *)
(* ================================================================== *)

let input_of_string s = Array.init (String.length s) (fun i -> Char.code s.[i])

let format_binding input_str { name; start_offset; end_offset } =
  if start_offset < 0 || end_offset < 0 then Printf.sprintf "%s=<unset>" name
  else if
    end_offset < start_offset
    || start_offset > String.length input_str
    || end_offset > String.length input_str
  then Printf.sprintf "%s=<invalid %d..%d>" name start_offset end_offset
  else
    Printf.sprintf "%s=%S" name
      (String.sub input_str start_offset (end_offset - start_offset))

let convert_ocamllex_result (r : Ocamllex_oracle.result option) : result option
    =
  match r with
    | None -> None
    | Some r ->
        Some
          {
            rule = r.rule;
            length = r.length;
            bindings =
              List.map
                (fun (b : Ocamllex_oracle.binding) ->
                  {
                    name = b.name;
                    start_offset = b.start_offset;
                    end_offset = b.end_offset;
                  })
                r.bindings;
          }

let run_all irs input_str =
  let compiled = Sedlex.compile_ir irs in
  let input = input_of_string input_str in
  let ocamllex = convert_ocamllex_result (Ocamllex_oracle.simulate irs input) in
  let dfa = DFA.simulate_ir compiled input in
  (ocamllex, dfa)

let check irs input_str =
  let ocamllex, dfa = run_all irs input_str in
  ocamllex = dfa

let oracle irs input_str =
  let ocamllex, dfa = run_all irs input_str in
  let fmt_result = function
    | None -> "no match"
    | Some r ->
        let fmt_bindings b =
          String.concat ", " (List.map (format_binding input_str) b)
        in
        Printf.sprintf "rule %d, len %d, [%s]" r.rule r.length
          (fmt_bindings r.bindings)
  in
  if ocamllex = dfa then Printf.printf "%S -> %s\n" input_str (fmt_result dfa)
  else
    Printf.printf "ERROR %S -> ocamllex=%s dfa=%s\n" input_str
      (fmt_result ocamllex) (fmt_result dfa)

(* ================================================================== *)
(* QCheck: random regexp + random input                               *)
(* ================================================================== *)

module G = QCheck2.Gen

(* Generator for random Ir.t patterns with captures. *)
let gen_ir : Ir.t G.t =
  let leaf =
    G.oneof_weighted
      [
        ( 3,
          G.map
            (fun c -> Ir.chars (Cset.singleton (Char.code c)))
            (G.oneof_list ['a'; 'b'; 'c'; 'd']) );
        ( 1,
          G.map2
            (fun a b -> Ir.chars (Cset.interval (Char.code a) (Char.code b)))
            (G.oneof_list ['a'; 'b'])
            (G.oneof_list ['c'; 'd']) );
      ]
  in
  let names = [| "x"; "y"; "z" |] in
  (* gen_inner: no Capture (safe for Star/Plus/Rep bodies) *)
  let gen_inner =
    G.sized
    @@ G.fix (fun self n ->
        if n <= 0 then leaf
        else (
          let child = self (n / 2) in
          G.oneof_weighted
            [
              (3, leaf);
              (2, G.map2 (fun a b -> Ir.seq a b) child child);
              (1, G.map2 (fun a b -> alt a b) child child);
              (1, G.map (fun r -> plus r) (self (n / 3)));
              (1, G.map (fun r -> star r) (self (n / 3)));
              (1, G.map (fun r -> opt r) (self (n / 3)));
              ( 1,
                G.map2
                  (fun lo hi ->
                    let lo = min lo hi and hi = max lo hi in
                    rep (cls 'a' 'c') lo hi)
                  (G.int_range 0 3) (G.int_range 0 3) );
              (1, G.return (sub (cls 'a' 'd') (cls 'c' 'd')));
              (1, G.return (inter (cls 'a' 'd') (cls 'b' 'c')));
              (1, G.return (compl (cls 'a' 'c')));
            ]))
  in
  (* Safe combinators for the generator: fall back gracefully when IR
     validation rejects a combination (e.g. Alt with mismatched captures). *)
  let safe_alt a b =
    match Ir.alt a b with Ok t -> t | Error _ -> Ir.seq a b
  in
  let safe_capture name r =
    match Ir.capture name r with Ok t -> t | Error _ -> r
  in
  (* gen_top: may contain Capture wrapping gen_inner, plus all operators. *)
  let name_i = ref 0 in
  G.sized
  @@ G.fix (fun self n ->
      if n <= 0 then leaf
      else (
        let sub_top = self (n / 2) in
        G.oneof_weighted
          [
            (3, leaf);
            (3, G.map2 (fun a b -> Ir.seq a b) sub_top sub_top);
            (1, G.map2 (fun a b -> safe_alt a b) sub_top sub_top);
            (1, G.map (fun r -> star r) gen_inner);
            (1, G.map (fun r -> plus r) gen_inner);
            (1, G.map (fun r -> opt r) gen_inner);
            ( 2,
              G.map
                (fun r ->
                  let i = !name_i mod Array.length names in
                  incr name_i;
                  safe_capture names.(i) r)
                gen_inner );
          ]))

let gen_input =
  G.string_size ~gen:(G.oneof_list ['a'; 'b'; 'c'; 'd'; 'e']) (G.int_range 0 15)

let print_case (irs, input_str) =
  let rules =
    Array.to_list irs
    |> List.mapi (fun i r -> Printf.sprintf "  rule%d: %s" i (Ir.show r))
    |> String.concat "\n"
  in
  Printf.sprintf "rules:\n%s\ninput: %S" rules input_str
