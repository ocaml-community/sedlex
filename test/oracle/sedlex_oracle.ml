(* NFA/DFA oracle: verify that determinization preserves the language
   and that tagged transitions (for as-bindings) record correct positions.

   Note: this oracle always allocates both start and end tags for every
   [as] binding (via [Sedlex.bind]). The real PPX is smarter: it uses
   [bind_start_only] or [bind_end_only] when one boundary can be computed
   from a known offset (e.g. fixed-length sub-patterns), and skips tags
   entirely when both boundaries are known. As a result, the oracle
   exercises the general two-tag path but not the optimized single-tag
   or zero-tag paths that the PPX generates in practice. *)

open Sedlex_compiler

(* ================================================================== *)
(* Regexp description with named captures                             *)
(* ================================================================== *)

type desc =
  | Lit of char
  | Seq of desc * desc
  | Alt of desc * desc
  | Star of desc
  | Plus of desc
  | Opt of desc
  | Rep of desc * int * int (* bounded repetition: Rep(r, min, max) *)
  | Class of char * char
  | Compl of desc (* complement of a char class *)
  | Sub of desc * desc (* char class subtraction *)
  | Inter of desc * desc (* char class intersection *)
  | Bind of string * desc (* named capture group *)

(* Concise builders *)
let lit c = Lit c
let cls a b = Class (a, b)
let seq a b = Seq (a, b)
let alt a b = Alt (a, b)
let star r = Star r
let plus r = Plus r
let opt r = Opt r
let rep r lo hi = Rep (r, lo, hi)
let compl r = Compl r
let sub r1 r2 = Sub (r1, r2)
let inter r1 r2 = Inter (r1, r2)
let capture name r = Bind (name, r)
let ( ^. ) a b = Seq (a, b) (* sequence operator *)

let rec pp fmt = function
  | Lit c -> Format.fprintf fmt "%c" c
  | Seq (a, b) -> Format.fprintf fmt "(%a, %a)" pp a pp b
  | Alt (a, b) -> Format.fprintf fmt "(%a | %a)" pp a pp b
  | Star r -> Format.fprintf fmt "%a*" pp r
  | Plus r -> Format.fprintf fmt "%a+" pp r
  | Opt r -> Format.fprintf fmt "%a?" pp r
  | Rep (r, lo, hi) -> Format.fprintf fmt "%a{%d,%d}" pp r lo hi
  | Class (a, b) -> Format.fprintf fmt "[%c-%c]" a b
  | Compl r -> Format.fprintf fmt "~%a" pp r
  | Sub (a, b) -> Format.fprintf fmt "(%a \\ %a)" pp a pp b
  | Inter (a, b) -> Format.fprintf fmt "(%a & %a)" pp a pp b
  | Bind (name, r) -> Format.fprintf fmt "(%a as %s)" pp r name

let show d = Format.asprintf "%a" pp d

(* Compile a desc to a Sedlex.regexp, returning bind info *)
type bind_info = {
  name : string;
  start_cell : int;
  end_cell : int;
  disc : (int * int) option;
      (** [(disc_cell, value)] when this is one branch of an or-pattern *)
}

let compile_desc (d : desc) : Sedlex.regexp * bind_info list =
  let char_ c = Sedlex.chars (Cset.singleton (Char.code c)) in
  let range_ a b = Sedlex.chars (Cset.interval (Char.code a) (Char.code b)) in
  let require_some loc = function
    | Some r -> r
    | None ->
        failwith (Printf.sprintf "%s: operand must be a single-char regexp" loc)
  in
  let rec repeat r = function
    | 0, 0 -> Sedlex.eps
    | 0, m -> Sedlex.alt Sedlex.eps (Sedlex.seq r (repeat r (0, m - 1)))
    | n, m -> Sedlex.seq r (repeat r (n - 1, m - 1))
  in
  (* go returns (regexp, bind_info list) for its subtree *)
  let rec go = function
    | Lit c -> (char_ c, [])
    | Seq (a, b) ->
        let r1, t1 = go a in
        let r2, t2 = go b in
        (Sedlex.seq r1 r2, t1 @ t2)
    | Alt (a, b) ->
        let r1, t1 = go a in
        let r2, t2 = go b in
        if t1 = t2 then (Sedlex.alt r1 r2, t1)
        else begin
          let disc_cell = Sedlex.new_disc_cell () in
          let r1 = Sedlex.bind_disc r1 disc_cell 0 in
          let r2 = Sedlex.bind_disc r2 disc_cell 1 in
          let stamp v tags =
            List.map (fun bi -> { bi with disc = Some (disc_cell, v) }) tags
          in
          (Sedlex.alt r1 r2, stamp 0 t1 @ stamp 1 t2)
        end
    | Star r -> (Sedlex.rep (go r |> fst), [])
    | Plus r -> (Sedlex.plus (go r |> fst), [])
    | Opt r ->
        let r, tags = go r in
        (Sedlex.alt Sedlex.eps r, tags)
    | Rep (r, lo, hi) -> (repeat (go r |> fst) (lo, hi), [])
    | Class (a, b) -> (range_ a b, [])
    | Compl r -> (require_some "Compl" (Sedlex.compl (go r |> fst)), [])
    | Sub (a, b) ->
        (require_some "Sub" (Sedlex.subtract (go a |> fst) (go b |> fst)), [])
    | Inter (a, b) ->
        ( require_some "Inter" (Sedlex.intersection (go a |> fst) (go b |> fst)),
          [] )
    | Bind (name, inner) ->
        let r, tags = go inner in
        let r, s, e = Sedlex.bind r in
        (r, { name; start_cell = s; end_cell = e; disc = None } :: tags)
  in
  let r, binds = go d in
  (r, List.rev binds)

(* ================================================================== *)
(* NFA and DFA simulators                                             *)
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

let resolve_bindings tags binds =
  List.filter_map
    (fun bi ->
      let active =
        match bi.disc with
          | None -> true
          | Some (cell, value) -> tags.(cell) = encode_value value
      in
      if not active then None
      else
        Some
          {
            name = bi.name;
            start_offset = tags.(bi.start_cell);
            end_offset = tags.(bi.end_cell);
          })
    binds
  |> List.sort compare

module NFA = struct
  (* A thread is an NFA node with its own copy of the tag registers. *)
  type thread = { node : Sedlex.node; tags : int array }

  (* Epsilon closure with per-thread tags. Each node is visited at most once
     (first-path-wins priority). Tags are copied at each fork so different
     paths don't interfere. *)
  let eps_closure (threads : thread list) pos =
    let visited = Hashtbl.create 16 in
    let result = ref [] in
    let rec visit tags (node : Sedlex.node) =
      if not (Hashtbl.mem visited node.id) then begin
        Hashtbl.add visited node.id ();
        let tags = Array.copy tags in
        (match node.tag with Some op -> exec_tag tags pos op | None -> ());
        result := { node; tags } :: !result;
        List.iter (visit tags) node.eps
      end
    in
    List.iter (fun t -> visit t.tags t.node) threads;
    List.rev !result

  (* Simulate NFA with longest-match / first-rule-wins. *)
  let simulate rules num_tags (binds_per_rule : bind_info list array) input =
    let finals = Array.map snd rules in
    let init_tags = Array.make num_tags unset in
    let entries =
      List.map
        (fun (entry, _) -> { node = entry; tags = Array.copy init_tags })
        (Array.to_list rules)
    in
    let state = ref (eps_closure entries 0) in
    let accepting () =
      let r = ref None in
      for i = 0 to Array.length finals - 1 do
        if !r = None then (
          match List.find_opt (fun t -> t.node == finals.(i)) !state with
            | Some t -> r := Some (i, t.tags)
            | None -> ())
      done;
      !r
    in
    let last = ref None in
    let pos = ref 0 in
    let continue = ref true in
    while !continue do
      (match accepting () with
        | Some (r, tags) ->
            let bindings = resolve_bindings tags binds_per_rule.(r) in
            last := Some { rule = r; length = !pos; bindings }
        | None -> ());
      if !pos >= Array.length input then continue := false
      else begin
        let c = input.(!pos) in
        let targets = ref [] in
        List.iter
          (fun t ->
            List.iter
              (fun (cset, tgt) ->
                if Cset.mem c cset then
                  targets := { node = tgt; tags = t.tags } :: !targets)
              t.node.trans)
          !state;
        let next = eps_closure !targets (!pos + 1) in
        if next = [] then continue := false
        else (
          state := next;
          incr pos)
      end
    done;
    !last
end

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
  let simulate (compiled : Sedlex.compiled)
      (binds_per_rule : bind_info list array) input =
    let dfa = compiled.dfa in
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
              resolve_bindings (Array.copy tags) binds_per_rule.(r)
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
(* Oracle: run NFA + DFA, compare, extract + verify captures          *)
(* ================================================================== *)

let input_of_string s = Array.init (String.length s) (fun i -> Char.code s.[i])

let format_binding input_str { name; start_offset; end_offset; _ } =
  if start_offset < 0 || end_offset < 0 then Printf.sprintf "%s=<unset>" name
  else
    Printf.sprintf "%s=%S" name
      (String.sub input_str start_offset (end_offset - start_offset))

let oracle rules input_str =
  Sedlex.reset_tags ();
  let descs = Array.map (fun f -> f ()) rules in
  let compiled_descs = Array.map compile_desc descs in
  let regexps = Array.map fst compiled_descs in
  let binds_per_rule = Array.map snd compiled_descs in
  let compiled = Sedlex.compile regexps in
  let nfa_pairs = Array.map Sedlex.compile_re regexps in
  let input = input_of_string input_str in
  let nfa = NFA.simulate nfa_pairs compiled.num_tags binds_per_rule input in
  let dfa = DFA.simulate compiled binds_per_rule input in
  let fmt_result = function
    | None -> "no match"
    | Some r ->
        let fmt_bindings b =
          String.concat ", " (List.map (format_binding input_str) b)
        in
        Printf.sprintf "rule %d, len %d, [%s]" r.rule r.length
          (fmt_bindings r.bindings)
  in
  if nfa = dfa then Printf.printf "%S -> %s\n" input_str (fmt_result dfa)
  else
    Printf.printf "FIXME %S -> nfa=%s dfa=%s\n" input_str (fmt_result nfa)
      (fmt_result dfa)

(* ================================================================== *)
(* QCheck: random regexp + random input                               *)
(* ================================================================== *)

module G = QCheck2.Gen

(* Generator for random regexps with captures. Ambiguous boundaries are
   intentionally included — the oracle verifies submatch validity. *)
let gen_desc : desc G.t =
  let leaf =
    G.oneof_weighted
      [
        (3, G.map (fun c -> Lit c) (G.oneof_list ['a'; 'b'; 'c'; 'd']));
        ( 1,
          G.map2
            (fun a b -> Class (a, b))
            (G.oneof_list ['a'; 'b'])
            (G.oneof_list ['c'; 'd']) );
      ]
  in
  let names = [| "x"; "y"; "z" |] in
  (* gen_inner: no Bind (safe for Star/Plus/Opt/Rep bodies) *)
  let gen_inner =
    G.sized
    @@ G.fix (fun self n ->
        if n <= 0 then leaf
        else (
          let sub = self (n / 2) in
          G.oneof_weighted
            [
              (3, leaf);
              (2, G.map2 (fun a b -> Seq (a, b)) sub sub);
              (1, G.map2 (fun a b -> Alt (a, b)) sub sub);
              (1, G.map plus (self (n / 3)));
              (1, G.map star (self (n / 3)));
              (1, G.map opt (self (n / 3)));
              ( 1,
                G.map2
                  (fun lo hi ->
                    let lo = min lo hi and hi = max lo hi in
                    Rep (Class ('a', 'c'), lo, hi))
                  (G.int_range 0 3) (G.int_range 0 3) );
              (1, G.return (Sub (Class ('a', 'd'), Class ('c', 'd'))));
              (1, G.return (Inter (Class ('a', 'd'), Class ('b', 'c'))));
              (1, G.return (Compl (Class ('a', 'c'))));
            ]))
  in
  (* gen_top: may contain Bind wrapping gen_inner, plus all operators *)
  let name_i = ref 0 in
  G.sized
  @@ G.fix (fun self n ->
      if n <= 0 then leaf
      else (
        let sub = self (n / 2) in
        G.oneof_weighted
          [
            (3, leaf);
            (3, G.map2 (fun a b -> Seq (a, b)) sub sub);
            (1, G.map2 (fun a b -> Alt (a, b)) sub sub);
            (1, G.map star gen_inner);
            (1, G.map plus gen_inner);
            (1, G.map opt gen_inner);
            ( 2,
              G.map
                (fun r ->
                  let i = !name_i mod Array.length names in
                  incr name_i;
                  Bind (names.(i), r))
                gen_inner );
          ]))

let gen_input =
  G.string_size ~gen:(G.oneof_list ['a'; 'b'; 'c'; 'd'; 'e']) (G.int_range 0 15)

(* QCheck oracle: check NFA/DFA agree. When [~check_tags] is true,
   also verify that bindings match between NFA and DFA. *)
let qcheck_oracle ~check_tags (descs, input_str) =
  Sedlex.reset_tags ();
  let compiled_descs = Array.map compile_desc descs in
  let regexps = Array.map fst compiled_descs in
  let binds_per_rule = Array.map snd compiled_descs in
  let compiled = Sedlex.compile regexps in
  let nfa_pairs = Array.map Sedlex.compile_re regexps in
  let input = input_of_string input_str in
  let nfa = NFA.simulate nfa_pairs compiled.num_tags binds_per_rule input in
  let dfa = DFA.simulate compiled binds_per_rule input in
  match (nfa, dfa) with
    | None, None -> true
    | Some n, Some d ->
        n.rule = d.rule && n.length = d.length
        && ((not check_tags) || n.bindings = d.bindings)
    | _ -> false

let print_case (descs, input_str) =
  let rules =
    Array.to_list descs
    |> List.mapi (fun i r -> Printf.sprintf "  rule%d: %s" i (show r))
    |> String.concat "\n"
  in
  Printf.sprintf "rules:\n%s\ninput: %S" rules input_str
