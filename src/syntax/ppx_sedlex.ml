(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

open Ppxlib
open Ast_builder.Default

(* let ocaml_version = Versions.ocaml_408 *)

module Cset = Sedlex_cset

(* Decision tree for partitions *)

let default_loc = Location.none
let lident_loc ~loc s = { loc; txt = lident s }

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

let limit = 8192

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

let segments_of_partition p =
  let seg = ref [] in
  Array.iteri
    (fun i c -> List.iter (fun (a, b) -> seg := (a, b, i) :: !seg) c)
    p;
  List.sort (fun (a1, _, _) (a2, _, _) -> compare a1 a2) !seg

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

(* Lexeme aliases *)

module StrLocSet = Set.Make (struct
  type t = string loc

  let compare a b = compare a.txt b.txt
end)

let builtin_regexps =
  List.fold_left
    (fun acc (n, c) -> StringMap.add n (Sedlex.chars c) acc)
    StringMap.empty
    ([
       ("any", Cset.any);
       ("eof", Cset.eof);
       ("xml_letter", Cset.letter);
       ("xml_digit", Cset.digit);
       ("xml_extender", Cset.extender);
       ("xml_base_char", Cset.base_char);
       ("xml_ideographic", Cset.ideographic);
       ("xml_combining_char", Cset.combining_char);
       ("xml_blank", Cset.blank);
       ("tr8876_ident_char", Cset.tr8876_ident_char);
     ]
    @ Unicode.Categories.list @ Unicode.Properties.list)

(* Tables (indexed mapping: codepoint -> next state) *)

let tables = Hashtbl.create 31
let table_counter = ref 0
let get_tables () = Hashtbl.fold (fun key x accu -> (x, key) :: accu) tables []

let table_name x =
  try Hashtbl.find tables x
  with Not_found ->
    incr table_counter;
    let s = Printf.sprintf "__sedlex_table_%i" !table_counter in
    Hashtbl.add tables x s;
    s

let table (name, v) =
  let n = Array.length v in
  let s = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.set s i (Char.chr v.(i))
  done;
  glb_value name (estring ~loc:default_loc (Bytes.to_string s))

(* Partition (function: codepoint -> next state) *)

let partitions = Hashtbl.create 31
let partition_counter = ref 0

let get_partitions () =
  Hashtbl.fold (fun key x accu -> (x, key) :: accu) partitions []

let partition_name x =
  try Hashtbl.find partitions x
  with Not_found ->
    incr partition_counter;
    let s = Printf.sprintf "__sedlex_partition_%i" !partition_counter in
    Hashtbl.add partitions x s;
    s

(* We duplicate the body for the EOF (-1) case rather than creating
   an interior utility function. *)
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
  glb_value name [%expr fun c -> [%e body]]

(* Code generation for the automata *)

let best_final final =
  let fin = ref None in
  for i = Array.length final - 1 downto 0 do
    if final.(i) then fin := Some i
  done;
  !fin

let state_fun state = Printf.sprintf "__sedlex_state_%i" state
let trace_fun i = Printf.sprintf "__sedlex_trace_%i" i

let call_state lexbuf auto state =
  let loc = default_loc in
  let trans, final = auto.(state) in
  if Array.length trans = 0 then (
    match best_final final with
      | Some i ->
          [%expr [%e eint ~loc i], [%e eint ~loc state] :: __sedlex_path]
      | None -> assert false)
  else
    appfun (state_fun state)
      [[%expr [%e eint ~loc state] :: __sedlex_path]; evar ~loc lexbuf]

let gen_state lexbuf auto i (trans, final) =
  let loc = default_loc in
  let partition = Array.map fst trans in
  let cases =
    Array.mapi
      (fun i (_, j) ->
        case ~lhs:(pint ~loc i) ~guard:None ~rhs:(call_state lexbuf auto j))
      trans
  in
  let cases = Array.to_list cases in
  let body () =
    pexp_match ~loc
      (appfun (partition_name partition)
         [[%expr Sedlexing.__private__next_int [%e evar ~loc lexbuf]]])
      (cases
      @ [
          case
            ~lhs:[%pat? _]
            ~guard:None
            ~rhs:[%expr Sedlexing.backtrack [%e evar ~loc lexbuf]];
        ])
  in
  let ret body =
    [
      value_binding ~loc
        ~pat:(pvar ~loc (state_fun i))
        ~expr:[%expr fun __sedlex_path [%p pvar ~loc lexbuf] -> [%e body]];
    ]
  in
  match best_final final with
    | None -> ret (body ())
    | Some _ when Array.length trans = 0 -> []
    | Some i ->
        ret
          [%expr
            Sedlexing.mark [%e evar ~loc lexbuf] [%e eint ~loc i] __sedlex_path;
            [%e body ()]]

let gen_recflag auto =
  (* The generated function is not recursive if the transitions end
     in states with no further transitions. *)
  try
    Array.iter
      (fun (trans_i, _) ->
        Array.iter
          (fun (_, j) ->
            let trans_j, _ = auto.(j) in
            if Array.length trans_j > 0 then raise Exit)
          trans_i)
      auto;
    Nonrecursive
  with Exit -> Recursive

let gen_offsets traces i = function
  | (_, []), _ -> None
  | (_, aliases), _ ->
      let n = List.length aliases in
      let _, trans, finals = traces.(i) in
      let cases =
        List.map (fun ({ actions; _ } : Sedlex.trans_case) -> actions) trans
        @ List.map (fun ({ actions; _ } : Sedlex.final_case) -> actions) finals
      in
      let action2cases = Hashtbl.create n in
      List.iteri
        (fun i actions ->
          List.iter
            (fun action ->
              try
                let offsets = Hashtbl.find action2cases action in
                Hashtbl.replace action2cases action (i :: offsets)
              with Not_found -> Hashtbl.add action2cases action [i])
            actions)
        cases;
      let counter = ref 0 in
      let alias2offset = Hashtbl.create n in
      let cases2offset = Hashtbl.create 31 in
      Hashtbl.iter
        (fun action offsets ->
          try
            let i = Hashtbl.find cases2offset offsets in
            Hashtbl.replace cases2offset offsets i;
            Hashtbl.add alias2offset action i
          with Not_found ->
            Hashtbl.add cases2offset offsets !counter;
            Hashtbl.add alias2offset action !counter;
            incr counter)
        action2cases;
      Some (!counter, alias2offset)

let gen_cset ~loc x char_set =
  let interval a b =
    [%expr
      [%e eint ~loc a] <= [%e evar ~loc x]
      && [%e evar ~loc x] <= [%e eint ~loc b]]
  in
  match char_set with
    | (a, b) :: l ->
        List.fold_left
          (fun acc (a, b) -> [%expr [%e acc] || [%e interval a b]])
          (interval a b) l
    | [] -> assert false

let gen_trace lexbuf traces i = function
  | None -> []
  | Some (offsets_num, action_offsets) ->
      let loc = default_loc in
      let initial, trans, finals = traces.(i) in
      let offset_array =
        value_binding ~loc
          ~pat:[%pat? __sedlex_offsets]
          ~expr:(pexp_array ~loc (List.init offsets_num (fun _ -> [%expr 0])))
      in
      let find_offset_idx action = Hashtbl.find action_offsets action in
      let aux_fun =
        let gen_action e action =
          let offset_idx = find_offset_idx action in
          [%expr
            __sedlex_offsets.([%e eint ~loc offset_idx]) <- __sedlex_pos;
            [%e e]]
        in
        let unreachable_case =
          case ~lhs:[%pat? _] ~guard:None ~rhs:[%expr assert false]
        in
        let trans_cases =
          let dup_case = Hashtbl.create (List.length trans) in
          List.iter
            (fun { Sedlex.curr_state; curr_node; prev_state; _ } ->
              let key = (curr_state, curr_node, prev_state) in
              try
                ignore (Hashtbl.find dup_case key);
                Hashtbl.replace dup_case key false
              with Not_found -> Hashtbl.add dup_case key true)
            trans;
          List.map
            (fun {
                   Sedlex.curr_state;
                   curr_node;
                   prev_state;
                   prev_node;
                   char_set;
                   actions;
                 } ->
              let lhs =
                ppat_tuple ~loc
                  [
                    pint ~loc curr_state;
                    pint ~loc curr_node;
                    pint ~loc prev_state;
                  ]
              in
              let guard =
                if Hashtbl.find dup_case (curr_state, curr_node, prev_state)
                then None
                else Some (gen_cset ~loc "__sedlex_code" char_set)
              in
              let rhs =
                let call_rest =
                  [%expr
                    __sedlex_aux (__sedlex_pos - 1) [%e eint ~loc prev_state]
                      [%e eint ~loc prev_node] __sedlex_rest]
                in
                List.fold_left gen_action call_rest actions
              in
              case ~lhs ~guard ~rhs)
            trans
        in
        let final_cases =
          List.map
            (fun { Sedlex.curr_node; actions } ->
              let lhs = pint ~loc curr_node in
              let rhs = List.fold_left gen_action [%expr ()] actions in
              case ~lhs ~guard:None ~rhs)
            finals
        in
        value_binding ~loc
          ~pat:[%pat? __sedlex_aux]
          ~expr:
            [%expr
              fun __sedlex_pos __sedlex_curr_state __sedlex_curr_node ->
                function
                | [] ->
                    [%e
                      pexp_match ~loc [%expr __sedlex_curr_node]
                        (final_cases @ [unreachable_case])]
                | __sedlex_prev_state :: __sedlex_rest ->
                    let __sedlex_code =
                      Sedlexing.lexeme_code [%e evar ~loc lexbuf]
                        (__sedlex_pos - 1)
                    in
                    [%e
                      pexp_match ~loc
                        [%expr
                          __sedlex_curr_state,
                            __sedlex_curr_node,
                            __sedlex_prev_state]
                        (trans_cases @ [unreachable_case])]]
      in
      [
        value_binding ~loc
          ~pat:(pvar ~loc (trace_fun i))
          ~expr:
            [%expr
              fun [%p pvar ~loc lexbuf] __sedlex_path ->
                [%e
                  pexp_let ~loc Nonrecursive [offset_array]
                  @@ pexp_let ~loc Recursive [aux_fun]
                  @@ [%expr
                       (match __sedlex_path with
                         | __sedlex_curr_state :: __sedlex_rest ->
                             __sedlex_aux
                               (Sedlexing.lexeme_length [%e evar ~loc lexbuf])
                               __sedlex_curr_state [%e eint ~loc initial]
                               __sedlex_rest
                         | _ -> assert false);
                       __sedlex_offsets]]];
      ]

let gen_aliases lexbuf offsets i e = function
  | [] -> e
  | aliases ->
      let loc = default_loc in
      let _, action_offsets = Option.get offsets.(i) in
      pexp_let ~loc Nonrecursive
        [
          value_binding ~loc
            ~pat:[%pat? __sedlex_offsets]
            ~expr:
              (appfun (trace_fun i) [evar ~loc lexbuf; [%expr __sedlex_path]]);
        ]
      @@ pexp_let ~loc Nonrecursive
           (List.map
              (fun { txt = alias; loc } ->
                let start = Hashtbl.find action_offsets (alias, true) in
                let stop = Hashtbl.find action_offsets (alias, false) in
                value_binding ~loc ~pat:(pvar ~loc alias)
                  ~expr:
                    [%expr
                      __sedlex_offsets.([%e eint ~loc start]),
                        __sedlex_offsets.([%e eint ~loc stop])
                        - __sedlex_offsets.([%e eint ~loc start])])
              aliases)
      @@ e

let gen_definition lexbuf l error =
  let loc = default_loc in
  let brs =
    Array.of_list
      (List.map (fun ((r, s), e) -> ((r, StrLocSet.elements s), e)) l)
  in
  let auto, traces = Sedlex.compile (Array.map (fun ((r, _), _) -> r) brs) in
  let offsets = Array.mapi (gen_offsets traces) brs in
  let cases =
    Array.to_list
      (Array.mapi
         (fun i ((_, aliases), e) ->
           case ~lhs:(pint ~loc i) ~guard:None
             ~rhs:(gen_aliases lexbuf offsets i e aliases))
         brs)
  in
  let states = Array.mapi (gen_state lexbuf auto) auto in
  let states = List.flatten (Array.to_list states) in
  let traces = Array.mapi (gen_trace lexbuf traces) offsets in
  let traces = List.flatten (Array.to_list traces) in
  pexp_let ~loc (gen_recflag auto) (states @ traces)
  @@ [%expr
       Sedlexing.start [%e evar ~loc lexbuf];
       let __sedlex_result, __sedlex_path =
         [%e appfun (state_fun 0) [[%expr [0]]; evar ~loc lexbuf]]
       in
       [%e
         pexp_match ~loc [%expr __sedlex_result]
           (cases @ [case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:error])]]

(* Lexer specification parser *)

let codepoint i =
  if i < 0 || i > Cset.max_code then
    failwith (Printf.sprintf "Invalid Unicode code point: %i" i);
  i

let regexp_for_char c = Sedlex.chars (Cset.singleton (Char.code c))

let regexp_for_string s =
  let rec aux n =
    if n = String.length s then Sedlex.eps
    else Sedlex.seq (regexp_for_char s.[n]) (aux (succ n))
  in
  aux 0

let err loc s =
  raise (Location.Error (Location.Error.createf ~loc "Sedlex: %s" s))

let rec repeat r = function
  | 0, 0 -> Sedlex.eps
  | 0, m -> Sedlex.alt Sedlex.eps (Sedlex.seq r (repeat r (0, m - 1)))
  | n, m -> Sedlex.seq r (repeat r (n - 1, m - 1))

let regexp_of_pattern allow_alias env =
  let rec char_pair_op func name p tuple =
    (* Construct something like Sub(a,b) *)
    match tuple with
      | Some { ppat_desc = Ppat_tuple [p0; p1] } -> begin
          match func (fst @@ aux false p0) (fst @@ aux false p1) with
            | Some r -> (r, StrLocSet.empty)
            | None ->
                err p.ppat_loc @@ "the " ^ name
                ^ " operator can only applied to single-character length \
                   regexps"
        end
      | _ ->
          err p.ppat_loc @@ "the " ^ name
          ^ " operator requires two arguments, like " ^ name ^ "(a,b)"
  and aux allow_alias p =
    let loc = p.ppat_loc in
    (* interpret one pattern node *)
    match p.ppat_desc with
      | Ppat_or (p1, p2) ->
          let r1, s1 = aux allow_alias p1 in
          let r2, s2 = aux allow_alias p2 in
          if not (StrLocSet.equal s1 s2) then begin
            let x =
              try StrLocSet.choose (StrLocSet.diff s1 s2)
              with Not_found -> StrLocSet.choose (StrLocSet.diff s2 s1)
            in
            err loc @@ "variable " ^ x.txt
            ^ " must occur on both sides of this | pattern"
          end;
          (Sedlex.alt r1 r2, s1)
      | Ppat_tuple (p :: pl) ->
          List.fold_left
            (fun (r1, s1) p ->
              let r2, s2 = aux allow_alias p in
              if not (StrLocSet.disjoint s1 s2) then begin
                let x = StrLocSet.choose (StrLocSet.inter s1 s2) in
                err loc @@ "variable " ^ x.txt
                ^ " is bound several times in this matching"
              end;
              (Sedlex.seq r1 r2, StrLocSet.union s1 s2))
            (aux allow_alias p) pl
      | Ppat_construct ({ txt = Lident "Star" }, Some (_, p)) ->
          (Sedlex.rep (fst @@ aux false p), StrLocSet.empty)
      | Ppat_construct ({ txt = Lident "Plus" }, Some (_, p)) ->
          (Sedlex.plus (fst @@ aux false p), StrLocSet.empty)
      | Ppat_construct
          ( { txt = Lident "Rep" },
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
                        };
                      ];
                } ) ) -> begin
          match (i1, i2) with
            | Pconst_integer (i1, _), Pconst_integer (i2, _) ->
                let i1 = int_of_string i1 in
                let i2 = int_of_string i2 in
                if 0 <= i1 && i1 <= i2 then
                  (repeat (fst @@ aux false p0) (i1, i2), StrLocSet.empty)
                else err p.ppat_loc "Invalid range for Rep operator"
            | _ ->
                err p.ppat_loc "Rep must take an integer constant or interval"
        end
      | Ppat_construct ({ txt = Lident "Rep" }, _) ->
          err p.ppat_loc "the Rep operator takes 2 arguments"
      | Ppat_construct ({ txt = Lident "Opt" }, Some (_, p)) ->
          (Sedlex.alt Sedlex.eps (fst @@ aux false p), StrLocSet.empty)
      | Ppat_construct ({ txt = Lident "Compl" }, arg) -> begin
          match arg with
            | Some (_, p0) -> begin
                match Sedlex.compl (fst @@ aux false p0) with
                  | Some r -> (r, StrLocSet.empty)
                  | None ->
                      err p.ppat_loc
                        "the Compl operator can only applied to a \
                         single-character length regexp"
              end
            | _ -> err p.ppat_loc "the Compl operator requires an argument"
        end
      | Ppat_construct ({ txt = Lident "Sub" }, arg) ->
          char_pair_op Sedlex.subtract "Sub" p
            (Option.map (fun (_, arg) -> arg) arg)
      | Ppat_construct ({ txt = Lident "Intersect" }, arg) ->
          char_pair_op Sedlex.intersection "Intersect" p
            (Option.map (fun (_, arg) -> arg) arg)
      | Ppat_construct ({ txt = Lident "Chars" }, arg) -> (
          let const =
            match arg with
              | Some (_, { ppat_desc = Ppat_constant const }) -> Some const
              | _ -> None
          in
          match const with
            | Some (Pconst_string (s, _, _)) ->
                let c = ref Cset.empty in
                for i = 0 to String.length s - 1 do
                  c := Cset.union !c (Cset.singleton (Char.code s.[i]))
                done;
                (Sedlex.chars !c, StrLocSet.empty)
            | _ ->
                err p.ppat_loc "the Chars operator requires a string argument")
      | Ppat_interval (i_start, i_end) -> begin
          match (i_start, i_end) with
            | Pconst_char c1, Pconst_char c2 ->
                ( Sedlex.chars (Cset.interval (Char.code c1) (Char.code c2)),
                  StrLocSet.empty )
            | Pconst_integer (i1, _), Pconst_integer (i2, _) ->
                ( Sedlex.chars
                    (Cset.interval
                       (codepoint (int_of_string i1))
                       (codepoint (int_of_string i2))),
                  StrLocSet.empty )
            | _ -> err p.ppat_loc "this pattern is not a valid interval regexp"
        end
      | Ppat_constant const -> begin
          match const with
            | Pconst_string (s, _, _) -> (regexp_for_string s, StrLocSet.empty)
            | Pconst_char c -> (regexp_for_char c, StrLocSet.empty)
            | Pconst_integer (i, _) ->
                ( Sedlex.chars (Cset.singleton (codepoint (int_of_string i))),
                  StrLocSet.empty )
            | _ -> err p.ppat_loc "this pattern is not a valid regexp"
        end
      | Ppat_var { txt = x } -> begin
          try (StringMap.find x env, StrLocSet.empty)
          with Not_found ->
            err p.ppat_loc (Printf.sprintf "unbound regexp %s" x)
        end
      | Ppat_alias (p, ({ txt = x } as x_loc)) when allow_alias ->
          let r, s = aux allow_alias p in
          if StrLocSet.mem x_loc s then begin
            err loc @@ "variable " ^ x
            ^ " is bound several times in this matching"
          end;
          (Sedlex.alias r x, StrLocSet.add x_loc s)
      | _ -> err p.ppat_loc "this pattern is not a valid regexp"
  in
  aux allow_alias

let previous = ref []
let regexps = ref []
let should_set_cookies = ref false

let mapper =
  object (this)
    inherit Ast_traverse.map as super
    val env = builtin_regexps

    method define_regexp name p =
      {<env = StringMap.add name (fst @@ regexp_of_pattern false env p) env>}

    method! expression e =
      match e with
        | [%expr [%sedlex [%e? { pexp_desc = Pexp_match (lexbuf, cases) }]]] ->
            let lexbuf =
              match lexbuf with
                | { pexp_desc = Pexp_ident { txt = Lident lexbuf } } -> lexbuf
                | _ ->
                    err lexbuf.pexp_loc
                      "the matched expression must be a single identifier"
            in
            let cases = List.rev cases in
            let error =
              match List.hd cases with
                | { pc_lhs = [%pat? _]; pc_rhs = e; pc_guard = None } ->
                    super#expression e
                | { pc_lhs = p } ->
                    err p.ppat_loc
                      "the last branch must be a catch-all error case"
            in
            let cases = List.rev (List.tl cases) in
            let cases =
              List.map
                (function
                  | { pc_lhs = p; pc_rhs = e; pc_guard = None } ->
                      (regexp_of_pattern true env p, super#expression e)
                  | { pc_guard = Some e } ->
                      err e.pexp_loc "'when' guards are not supported")
                cases
            in
            gen_definition lexbuf cases error
        | [%expr
            let [%p? { ppat_desc = Ppat_var { txt = name } }] =
              [%sedlex.regexp? [%p? p]]
            in
            [%e? body]] ->
            (this#define_regexp name p)#expression body
        | [%expr [%sedlex [%e? _]]] ->
            err e.pexp_loc
              "the %sedlex extension is only recognized on match expressions"
        | _ -> super#expression e

    val toplevel = true

    method structure_with_regexps l =
      let mapper = ref this in
      let regexps = ref [] in
      let l =
        List.concat
          (List.map
             (function
               | [%stri
                   let [%p? { ppat_desc = Ppat_var { txt = name } }] =
                     [%sedlex.regexp? [%p? p]]] as i ->
                   regexps := i :: !regexps;
                   mapper := !mapper#define_regexp name p;
                   []
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

let pre_handler cookies =
  previous :=
    match Driver.Cookies.get cookies "sedlex.regexps" Ast_pattern.__ with
      | Some { pexp_desc = Pexp_extension (_, PStr l) } -> l
      | Some _ -> assert false
      | None -> []

let post_handler cookies =
  if !should_set_cookies then (
    let loc = default_loc in
    Driver.Cookies.set cookies "sedlex.regexps"
      (pexp_extension ~loc ({ loc; txt = "regexps" }, PStr !regexps)))

let extensions =
  [
    Extension.declare "sedlex" Extension.Context.expression
      Ast_pattern.(single_expr_payload __)
      (fun ~loc:_ ~path:_ expr -> mapper#expression expr);
  ]

let () =
  Driver.Cookies.add_handler pre_handler;
  Driver.Cookies.add_post_handler post_handler;
  Driver.register_transformation "sedlex" ~impl:mapper#structure
