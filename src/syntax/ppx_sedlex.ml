(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

open Ppxlib
open Ast_builder.Default
open Ast_helper

(* let ocaml_version = Versions.ocaml_408 *)

module Cset = Sedlex_cset

(* Decision tree for partitions *)

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
    (fun i c ->
      List.iter
        (fun (a, b) -> seg := (a, b, i) :: !seg)
        (c : Sedlex_cset.t :> (int * int) list))
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

let builtin_regexps =
  List.fold_left
    (fun acc (n, c) -> StringMap.add n (Sedlex.chars c) acc)
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
  glb_value name
    [%expr
      fun c ->
        let open! Stdlib in
        [%e body]]

(* Code generation for the automata *)

let best_final final =
  let fin = ref None in
  for i = Array.length final - 1 downto 0 do
    if final.(i) then fin := Some i
  done;
  !fin

let state_fun state = Printf.sprintf "__sedlex_state_%i" state

let call_state lexbuf (auto : Sedlex.dfa) state =
  let { Sedlex.trans; finals } = auto.(state) in
  if Array.length trans = 0 then (
    match best_final finals with
      | Some i -> eint ~loc:default_loc i
      | None -> assert false)
  else appfun (state_fun state) [lexbuf]

let gen_state (lexbuf_name, lexbuf) (auto : Sedlex.dfa) i
    { Sedlex.trans; finals } =
  let loc = default_loc in
  let partition = Array.map (fun (cs, _, _) -> cs) trans in
  let cases =
    Array.mapi
      (fun i (_, j, tags) ->
        let rhs =
          List.fold_right
            (fun (op : Sedlex.tag_op) acc ->
              match op with
                | Set_position t ->
                    [%expr
                      Sedlexing.__private__set_mem_pos [%e lexbuf]
                        [%e eint ~loc t];
                      [%e acc]]
                | Set_value (cell, value) ->
                    [%expr
                      Sedlexing.__private__set_mem_value [%e lexbuf]
                        [%e eint ~loc cell] [%e eint ~loc value];
                      [%e acc]])
            tags (call_state lexbuf auto j)
        in
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

let gen_recflag (auto : Sedlex.dfa) =
  (* The generated function is not recursive if the transitions end
     in states with no further transitions. *)
  try
    Array.iter
      (fun { Sedlex.trans } ->
        Array.iter
          (fun (_, j, _) ->
            if Array.length auto.(j).Sedlex.trans > 0 then raise Exit)
          trans)
      auto;
    Nonrecursive
  with Exit -> Recursive

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
        List.fold_right
          (fun (op : Sedlex.tag_op) acc ->
            match op with
              | Set_position t ->
                  [%expr
                    Sedlexing.__private__set_mem_pos [%e lexbuf]
                      [%e eint ~loc t];
                    [%e acc]]
              | Set_value (cell, value) ->
                  [%expr
                    Sedlexing.__private__set_mem_value [%e lexbuf]
                      [%e eint ~loc cell] [%e eint ~loc value];
                    [%e acc]])
          compiled.init_tags
          (appfun (state_fun 0) [lexbuf])
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

let rec repeat r = function
  | 0, 0 -> Sedlex.eps
  | 0, m -> Sedlex.alt Sedlex.eps (Sedlex.seq r (repeat r (0, m - 1)))
  | n, m -> Sedlex.seq r (repeat r (n - 1, m - 1))

type tag_info = {
  name : string;
  start_tag : int;
  end_tag : int;
  disc : (int * int) list;
}

let gen_sub_lexeme lexbuf st et =
  let loc = default_loc in
  [%expr
    let __s = Sedlexing.__private__mem_pos [%e lexbuf] [%e eint ~loc st] in
    let __e = Sedlexing.__private__mem_pos [%e lexbuf] [%e eint ~loc et] in
    { Sedlexing.lexbuf = [%e lexbuf]; pos = __s; len = __e - __s }]

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
        (fun { name; start_tag; end_tag; disc } ->
          if not (Hashtbl.mem tbl name) then order := name :: !order;
          let existing = try Hashtbl.find tbl name with Not_found -> [] in
          Hashtbl.replace tbl name (existing @ [(start_tag, end_tag, disc)]))
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

let regexp_of_pattern env =
  let no_tags r = (r, ([] : tag_info list)) in
  let reject_tags loc ctx (r, tags) =
    if tags <> [] then err loc "'as' bindings are not supported inside %s" ctx;
    r
  in
  let rec char_pair_op func name ~encoding p tuple =
    (* Construct something like Sub(a,b) *)
      match tuple with
      | Some { ppat_desc = Ppat_tuple [p0; p1] } -> begin
          let r0 = reject_tags p0.ppat_loc name (aux ~encoding p0) in
          let r1 = reject_tags p1.ppat_loc name (aux ~encoding p1) in
          match func r0 r1 with
            | Some r -> no_tags r
            | None ->
                err p.ppat_loc
                  "the %s operator can only applied to single-character length \
                   regexps"
                  name
        end
      | _ ->
          err p.ppat_loc "the %s operator requires two arguments, like %s(a,b)"
            name name
  and aux ~encoding p =
    (* interpret one pattern node *)
      match p.ppat_desc with
      | Ppat_alias (inner, { txt = name }) ->
          let r, tags = aux ~encoding inner in
          let wrapped, start_tag, end_tag = Sedlex.bind r in
          (wrapped, { name; start_tag; end_tag; disc = [] } :: tags)
      | Ppat_or (p1, p2) ->
          let r1, tags1 = aux ~encoding p1 in
          let r2, tags2 = aux ~encoding p2 in
          if tags1 <> [] || tags2 <> [] then begin
            let names tags =
              List.map (fun ti -> ti.name) tags |> List.sort_uniq String.compare
            in
            if names tags1 <> names tags2 then
              err p.ppat_loc
                "both sides of '|' must bind the same names with 'as'";
            let stamp disc_cell value tags =
              List.map
                (fun ti -> { ti with disc = (disc_cell, value) :: ti.disc })
                tags
            in
            let disc_cell = Sedlex.new_disc_cell () in
            let r1w = Sedlex.bind_disc r1 disc_cell 0 in
            let r2w = Sedlex.bind_disc r2 disc_cell 1 in
            ( Sedlex.alt r1w r2w,
              stamp disc_cell 0 tags1 @ stamp disc_cell 1 tags2 )
          end
          else (Sedlex.alt r1 r2, tags1 @ tags2)
      | Ppat_tuple (p :: pl) ->
          List.fold_left
            (fun (r, tags) p ->
              let r', tags' = aux ~encoding p in
              (Sedlex.seq r r', tags @ tags'))
            (aux ~encoding p) pl
      | Ppat_construct ({ txt = Lident "Star" }, Some (_, p)) ->
          let r = reject_tags p.ppat_loc "Star" (aux ~encoding p) in
          no_tags (Sedlex.rep r)
      | Ppat_construct ({ txt = Lident "Plus" }, Some (_, p)) ->
          let r = reject_tags p.ppat_loc "Plus" (aux ~encoding p) in
          no_tags (Sedlex.plus r)
      | Ppat_construct ({ txt = Lident "Utf8" }, Some (_, p)) ->
          aux ~encoding:Utf8 p
      | Ppat_construct ({ txt = Lident "Latin1" }, Some (_, p)) ->
          aux ~encoding:Latin1 p
      | Ppat_construct ({ txt = Lident "Ascii" }, Some (_, p)) ->
          aux ~encoding:Ascii p
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
          let r = reject_tags p0.ppat_loc "Rep" (aux ~encoding p0) in
          match (i1, i2) with
            | Pconst_integer (i1, _), Pconst_integer (i2, _) ->
                let i1 = int_of_string i1 in
                let i2 = int_of_string i2 in
                if 0 <= i1 && i1 <= i2 then no_tags (repeat r (i1, i2))
                else err p.ppat_loc "Invalid range for Rep operator"
            | _ ->
                err p.ppat_loc "Rep must take an integer constant or interval"
        end
      | Ppat_construct ({ txt = Lident "Rep" }, _) ->
          err p.ppat_loc "the Rep operator takes 2 arguments"
      | Ppat_construct ({ txt = Lident "Opt" }, Some (_, p)) ->
          let r = reject_tags p.ppat_loc "Opt" (aux ~encoding p) in
          no_tags (Sedlex.alt Sedlex.eps r)
      | Ppat_construct ({ txt = Lident "Compl" }, arg) -> begin
          match arg with
            | Some (_, p0) -> begin
                let r = reject_tags p0.ppat_loc "Compl" (aux ~encoding p0) in
                match Sedlex.compl r with
                  | Some r -> no_tags r
                  | None ->
                      err p.ppat_loc
                        "the Compl operator can only applied to a \
                         single-character length regexp"
              end
            | _ -> err p.ppat_loc "the Compl operator requires an argument"
        end
      | Ppat_construct ({ txt = Lident "Sub" }, arg) ->
          char_pair_op ~encoding Sedlex.subtract "Sub" p
            (Option.map (fun (_, arg) -> arg) arg)
      | Ppat_construct ({ txt = Lident "Intersect" }, arg) ->
          char_pair_op ~encoding Sedlex.intersection "Intersect" p
            (Option.map (fun (_, arg) -> arg) arg)
      | Ppat_construct ({ txt = Lident "Chars" }, arg) -> (
          let const =
            match arg with
              | Some (_, { ppat_desc = Ppat_constant const }) -> Some const
              | _ -> None
          in
          match const with
            | Some (Pconst_string (s, _, _)) ->
                let l = rev_csets_of_string ~loc:p.ppat_loc ~encoding s in
                let chars = List.fold_left Cset.union Cset.empty l in
                no_tags (Sedlex.chars chars)
            | _ ->
                err p.ppat_loc "the Chars operator requires a string argument")
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
      | Ppat_var { txt = x } -> begin
          try no_tags (StringMap.find x env)
          with Not_found -> err p.ppat_loc "unbound regexp %s" x
        end
      | _ -> err p.ppat_loc "this pattern is not a valid regexp"
  in
  aux ~encoding:Ascii

let handle_sedlex_match ~env ~map_rhs match_expr =
  let lexbuf =
    match match_expr with
      | { pexp_desc = Pexp_match (lexbuf, _) } -> (
          match lexbuf with
            | { pexp_desc = Pexp_ident { txt = Lident txt } } -> (txt, lexbuf)
            | _ ->
                err lexbuf.pexp_loc
                  "the matched expression must be a single identifier")
      | _ ->
          err match_expr.pexp_loc
            "the %%sedlex extension is only recognized on match expressions"
  in
  let cases =
    match match_expr with
      | { pexp_desc = Pexp_match (_, cases) } -> cases
      | _ -> assert false
  in
  let cases = List.rev cases in
  let error =
    match List.hd cases with
      | { pc_lhs = [%pat? _]; pc_rhs = e; pc_guard = None } -> map_rhs e
      | { pc_lhs = p } ->
          err p.ppat_loc "the last branch must be a catch-all error case"
  in
  let cases = List.rev (List.tl cases) in
  Sedlex.reset_tags ();
  let cases_parsed =
    List.map
      (function
        | { pc_lhs = p; pc_rhs = e; pc_guard = None } ->
            let regexp, tag_info = regexp_of_pattern env p in
            (regexp, tag_info, e)
        | { pc_guard = Some e } ->
            err e.pexp_loc "'when' guards are not supported")
      cases
  in
  let compiled =
    Sedlex.compile (Array.of_list (List.map (fun (r, _, _) -> r) cases_parsed))
  in
  (* map_rhs is called after compile so that nested match%sedlex blocks
     (which call reset_tags) cannot corrupt the outer tag counter. *)
  let cases =
    List.map
      (fun (_, tag_info, e) ->
        let action = gen_binding_code (snd lexbuf) tag_info (map_rhs e) in
        ((), action))
      cases_parsed
  in
  (gen_definition lexbuf compiled cases error, compiled.dfa)

let previous = ref []
let regexps = ref []
let should_set_cookies = ref false

let mapper =
  object (this)
    inherit Ast_traverse.map as super
    val env = builtin_regexps

    method define_regexp name p =
      let r, tags = regexp_of_pattern env p in
      if tags <> [] then
        err p.ppat_loc "'as' bindings are not allowed in regexp definitions";
      {<env = StringMap.add name r env>}

    method! expression e =
      match e with
        | [%expr [%sedlex [%e? { pexp_desc = Pexp_match _ } as match_expr]]] ->
            fst (handle_sedlex_match ~env ~map_rhs:this#expression match_expr)
        | [%expr
            let [%p? { ppat_desc = Ppat_var { txt = name } }] =
              [%sedlex.regexp? [%p? p]]
            in
            [%e? body]] ->
            (this#define_regexp name p)#expression body
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
