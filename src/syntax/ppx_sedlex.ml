(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

open Longident
open Migrate_parsetree
open Ast_405
open Parsetree
open Asttypes
open Ast_helper
open Ast_convenience_405

module Ast_mapper_class = Ast_mapper_class_405

let ocaml_version = Versions.ocaml_405

module Cset = Sedlex_cset

(* Decision tree for partitions *)

type decision_tree =
  | Lte of int * decision_tree * decision_tree
  | Table of int * int array
  | Return of int

let decision l =
  let l = List.map (fun (a, b, i) -> (a, b, Return i)) l in
  let rec merge2 = function
    | (a1, b1, d1) :: (a2, b2, d2) :: rest ->
        let x =
          if b1 + 1 = a2 then d2
          else Lte (a2 - 1, Return (-1), d2)
        in
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
    | ((a, b, i) as x)::rem when b < limit && i < 255->
        aux (min a m) (x :: accu) rem
    | rem -> m, accu, rem
  in
  let (min, table, rest) = aux max_int [] l in
  match table with
  | [] -> decision l
  | [(min, max, i)] ->
      Lte (min - 1, Return (-1), (Lte (max, Return i, decision rest)))
  | (_, max, _) :: _ ->
      let arr = Array.make (max - min + 1) 0 in
      let set (a, b, i) = for j = a to b do arr.(j - min) <- i + 1 done in
      List.iter set table;
      Lte (min - 1, Return (-1), Lte (max, Table (min, arr), decision rest))

let rec simplify min max = function
  | Lte (i,yes,no) ->
      if i >= max then simplify min max yes
      else if i < min then simplify min max no
      else Lte (i, simplify min i yes, simplify (i+1) max no)
  | x -> x

let segments_of_partition p =
  let seg = ref [] in
  Array.iteri
    (fun i c -> List.iter (fun (a, b) -> seg := (a, b, i) :: !seg) c)
    p;
  List.sort (fun (a1,_,_) (a2,_,_) -> compare a1 a2) !seg

let decision_table p =
  simplify (-1) (Cset.max_code) (decision_table (segments_of_partition p))


(* Helpers to build AST *)

let appfun s l = app (evar s) l
let glb_value name def = Str.value Nonrecursive [Vb.mk (pvar name) def]

(* Named regexps *)

module StringMap = Map.Make(struct
  type t = string
  let compare = compare
end)

let builtin_regexps =
  List.fold_left (fun acc (n, c) -> StringMap.add n (Sedlex.chars c) acc)
    StringMap.empty
    ([
     "any", Cset.any;
     "eof", Cset.eof;
     "xml_letter", Cset.letter;
     "xml_digit", Cset.digit;
     "xml_extender", Cset.extender;
     "xml_base_char", Cset.base_char;
     "xml_ideographic", Cset.ideographic;
     "xml_combining_char", Cset.combining_char;
     "xml_blank", Cset.blank;
     "tr8876_ident_char", Cset.tr8876_ident_char] @
     Unicode.Categories.list @
     Unicode.Properties.list)

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
  for i = 0 to n - 1 do Bytes.set s i (Char.chr v.(i)) done;
  glb_value name (str (Bytes.to_string s))

(* Partition (function: codepoint -> next state) *)

let partitions = Hashtbl.create 31
let partition_counter = ref 0
let get_partitions () = Hashtbl.fold (fun key x accu -> (x, key) :: accu) partitions []

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
  let rec gen_tree = function
    | Lte (i, yes, no) ->
        [%expr if c <= [%e int i] then [%e gen_tree yes] else [%e gen_tree no]]
    | Return i -> int i
    | Table (offset, t) ->
              let c = if offset = 0 then [%expr c] else [%expr c - [%e int offset]] in
        [%expr Char.code (String.get [%e evar (table_name t)] [%e c]) - 1]
  in
  let body = gen_tree (decision_table p) in
  glb_value name (func [(pconstr "Some" [pvar "uc"],
                         [%expr let c = Uchar.to_int uc in [%e body]]);
                        (pconstr "None" [],
                         [%expr let c = (-1) in [%e body]])])

(* Code generation for the automata *)

let best_final final =
  let fin = ref None in
  for i = Array.length final - 1 downto 0 do
    if final.(i) then fin := Some i
  done;
  !fin

let state_fun state = Printf.sprintf "__sedlex_state_%i" state

let call_state lexbuf auto state =
  let (trans, final) = auto.(state) in
  if Array.length trans = 0
  then match best_final final with
  | Some i -> int i
  | None -> assert false
  else appfun (state_fun state) [evar lexbuf]

let gen_state lexbuf auto i (trans, final) =
  let partition = Array.map fst trans in
  let cases = Array.mapi (fun i (_, j) -> Exp.case(pint i) (call_state lexbuf auto j)) trans in
  let cases = Array.to_list cases in
  let body () =
    Exp.match_
      (appfun (partition_name partition) [[%expr Sedlexing.next [%e evar lexbuf]]])
      (cases @ [Exp.case [%pat? _] [%expr Sedlexing.backtrack [%e evar lexbuf]]])
  in
  let ret body = [ Vb.mk (pvar (state_fun i)) (func [pvar lexbuf, body]) ] in
  match best_final final with
    | None -> ret (body ())
    | Some _ when Array.length trans = 0 -> []
    | Some i -> ret [%expr Sedlexing.mark [%e evar lexbuf] [%e int i]; [%e body ()]]

let gen_recflag auto =
  (* The generated function is not recursive if the transitions end
     in states with no further transitions. *)
  try
    Array.iter
      (fun (trans_i, _) ->
        Array.iter
          (fun (_, j) ->
            let (trans_j, _) = auto.(j) in
            if Array.length trans_j > 0 then raise Exit)
          trans_i)
      auto;
    Nonrecursive
  with
    Exit -> Recursive

let gen_definition lexbuf l error =
  let brs = Array.of_list l in
  let auto = Sedlex.compile (Array.map fst brs) in
  let cases = Array.to_list (Array.mapi (fun i (_, e) -> Exp.case (pint i) e) brs) in
  let states = Array.mapi (gen_state lexbuf auto) auto in
  let states = List.flatten (Array.to_list states) in
  Exp.let_ (gen_recflag auto) states
    (Exp.sequence
       [%expr Sedlexing.start [%e evar lexbuf]]
       (Exp.match_ (appfun (state_fun 0) [evar lexbuf])
          (cases @ [Exp.case (Pat.any ()) error])
       )
    )

(* Lexer specification parser *)

let codepoint i =
  if i < 0 || i > Cset.max_code then
    failwith (Printf.sprintf "Invalid Unicode code point: %i" i);
  i

let regexp_for_char c =
  Sedlex.chars (Cset.singleton (Char.code c))

let regexp_for_string s =
  let rec aux n =
    if n = String.length s then Sedlex.eps
    else
      Sedlex.seq (regexp_for_char s.[n]) (aux (succ n))
  in aux 0

let err loc s =
  raise (Location.Error (Location.error ~loc ("Sedlex: " ^ s)))

let rec repeat r = function
  | 0, 0 -> Sedlex.eps
  | 0, m -> Sedlex.alt Sedlex.eps (Sedlex.seq r (repeat r (0, m - 1)))
  | n, m -> Sedlex.seq r (repeat r (n - 1, m - 1))

let regexp_of_pattern env =
  let rec char_pair_op func name p tuple = (* Construct something like Sub(a,b) *)
    match tuple with
      | Some {ppat_desc=Ppat_tuple (p0 :: p1 :: [])} ->
        begin match func (aux p0) (aux p1) with
        | Some r -> r
        | None ->
          err p.ppat_loc @@
            "the "^name^" operator can only applied to single-character length regexps"
        end
      | _ -> err p.ppat_loc @@ "the "^name^" operator requires two arguments, like "^name^"(a,b)"
  and aux p = (* interpret one pattern node *)
    match p.ppat_desc with
    | Ppat_or (p1, p2) -> Sedlex.alt (aux p1) (aux p2)
    | Ppat_tuple (p :: pl) ->
        List.fold_left (fun r p -> Sedlex.seq r (aux p))
          (aux p)
          pl
    | Ppat_construct ({txt = Lident "Star"}, Some p) ->
        Sedlex.rep (aux p)
    | Ppat_construct ({txt = Lident "Plus"}, Some p) ->
        Sedlex.plus (aux p)
    | Ppat_construct
        ({txt = Lident "Rep"},
         Some {ppat_desc=Ppat_tuple[p0; {ppat_desc=Ppat_constant (i1 as i2)|Ppat_interval(i1, i2)}]}) ->
         begin match Constant.of_constant i1, Constant.of_constant i2 with
         | Pconst_integer(i1,_), Pconst_integer(i2,_) ->
             let i1 = int_of_string i1 in
             let i2 = int_of_string i2 in
             if 0 <= i1 && i1 <= i2 then repeat (aux p0) (i1, i2)
             else err p.ppat_loc "Invalid range for Rep operator"
         | _ ->
             err p.ppat_loc "Rep must take an integer constant or interval"
         end
    | Ppat_construct ({txt = Lident "Rep"}, _) ->
        err p.ppat_loc "the Rep operator takes 2 arguments"
    | Ppat_construct ({txt = Lident "Opt"}, Some p) ->
        Sedlex.alt Sedlex.eps (aux p)
    | Ppat_construct ({txt = Lident "Compl"}, arg) ->
        begin match arg with
        | Some p0 ->
            begin match Sedlex.compl (aux p0) with
            | Some r -> r
            | None ->
              err p.ppat_loc
                "the Compl operator can only applied to a single-character length regexp"
            end
        | _ -> err p.ppat_loc "the Compl operator requires an argument"
        end
    | Ppat_construct ({ txt = Lident "Sub" }, arg) ->
        char_pair_op Sedlex.subtract "Sub" p arg
    | Ppat_construct ({ txt = Lident "Intersect" }, arg) ->
        char_pair_op Sedlex.intersection "Intersect" p arg
    | Ppat_construct ({txt = Lident "Chars"}, arg) ->
        let const = match arg with
          | Some {ppat_desc=Ppat_constant const} ->
              Some (Constant.of_constant const)
          | _ -> None
        in
        begin match const with
        | Some (Pconst_string(s,_))->
            let c = ref Cset.empty in
            for i = 0 to String.length s - 1 do
              c := Cset.union !c (Cset.singleton (Char.code s.[i]))
            done;
            Sedlex.chars !c
        | _ -> err p.ppat_loc "the Chars operator requires a string argument"
        end
    | Ppat_interval (i_start, i_end) ->
        begin match Constant.of_constant i_start, Constant.of_constant i_end with
          | Pconst_char c1, Pconst_char c2 -> Sedlex.chars (Cset.interval (Char.code c1) (Char.code c2))
          | Pconst_integer(i1,_), Pconst_integer(i2,_) ->
              Sedlex.chars (Cset.interval (codepoint (int_of_string i1)) (codepoint (int_of_string i2)))
          | _ -> err p.ppat_loc "this pattern is not a valid interval regexp"
        end
    | Ppat_constant (const) ->
        begin match Constant.of_constant const with
          | Pconst_string (s, _) -> regexp_for_string s
          | Pconst_char c -> regexp_for_char c
          | Pconst_integer(i,_) -> Sedlex.chars (Cset.singleton (codepoint (int_of_string i)))
          | _ -> err p.ppat_loc "this pattern is not a valid regexp"
        end
    | Ppat_var {txt=x} ->
        begin try StringMap.find x env
        with Not_found ->
          err p.ppat_loc (Printf.sprintf "unbound regexp %s" x)
        end
    | _ ->
      err p.ppat_loc "this pattern is not a valid regexp"
  in
  aux


let mapper cookies =
  object(this)
    inherit Ast_mapper_class.mapper as super

    val env = builtin_regexps

    method define_regexp name p =
      {< env = StringMap.add name (regexp_of_pattern env p) env >}

    method! expr e =
      match e with
      | [%expr [%sedlex [%e? {pexp_desc=Pexp_match (lexbuf, cases)}]]] ->
            let lexbuf =
              match lexbuf with
              | {pexp_desc=Pexp_ident{txt=Lident lexbuf}} -> lexbuf
              | _ ->
                err lexbuf.pexp_loc "the matched expression must be a single identifier"
            in
            let cases = List.rev cases in
            let error =
              match List.hd cases with
              | {pc_lhs = [%pat? _]; pc_rhs = e; pc_guard = None} -> super # expr e
              | {pc_lhs = p} ->
                err p.ppat_loc "the last branch must be a catch-all error case"
            in
            let cases = List.rev (List.tl cases) in
            let cases =
              List.map
                (function
                  | {pc_lhs = p; pc_rhs = e; pc_guard = None} -> regexp_of_pattern env p, super # expr e
                  | {pc_guard = Some e} ->
                    err e.pexp_loc "'when' guards are not supported"
                ) cases
            in
            gen_definition lexbuf cases error
      | [%expr let [%p? {ppat_desc=Ppat_var{txt=name}}] = [%sedlex.regexp? [%p? p]] in [%e? body]] ->
          (this # define_regexp name p) # expr body
      | [%expr [%sedlex [%e? _]]] ->
        err e.pexp_loc "the %sedlex extension is only recognized on match expressions"
      | _ -> super # expr e


    val toplevel = true

    method structure_with_regexps l =
      let mapper = ref this in
      let regexps = ref [] in
      let l = List.concat
        (List.map
           (function
             | [%stri let [%p? {ppat_desc=Ppat_var{txt=name}}] = [%sedlex.regexp? [%p? p]]] as i ->
               regexps := i :: !regexps;
               mapper := !mapper # define_regexp name p;
               []
             | i ->
               [ !mapper # structure_item i ]
         ) l) in
      (l, List.rev !regexps)

    method! structure l =
      if toplevel then
        let sub = {< toplevel = false >} in
        let previous =
          match Driver.get_cookie cookies "sedlex.regexps" ocaml_version with
          | Some {pexp_desc = Pexp_extension (_, PStr l)} -> l
          | Some _ -> assert false
          | None -> []
        in
        let l, regexps = sub # structure_with_regexps (previous @ l) in
        let parts = List.map partition (get_partitions ()) in
        let tables = List.map table (get_tables ()) in
        Driver.set_cookie cookies "sedlex.regexps" ocaml_version (Exp.extension (Location.mknoloc "regexps", PStr regexps));
        tables @ parts @ l
      else
        fst (this # structure_with_regexps l)

 end

let () =
  Driver.register
    ~name:"sedlex"
    ocaml_version
    (fun _ cookies -> Ast_mapper_class.to_mapper (mapper cookies))
