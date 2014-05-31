(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

open Longident
open Parsetree
open Asttypes
open Ast_helper
open Ast_convenience

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
      let arr = Array.create (max - min + 1) 0 in
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
let pint i = Pat.constant (Const_int i)
let glb_value name def = Str.value Nonrecursive [Vb.mk (pvar name) def]

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

let partition (name, p) =
  let rec gen_tree = function
    | Lte (i, yes, no) ->
        Exp.ifthenelse
            (appfun "<=" [evar "c"; int i])
            (gen_tree yes)
            (Some (gen_tree no))
    | Return i -> int i
    | Table (offset, t) ->
        let c =
          if offset = 0 then evar "c"
          else appfun "-" [evar "c"; int offset]
        in
        appfun "-"
          [
           appfun "Char.code" [appfun "String.get" [evar (table_name t); c]];
           int 1;
          ]
  in
  let body = gen_tree (decision_table p) in
  glb_value name (func [pvar "c", body])

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
      (appfun (partition_name partition) [appfun "Sedlexing.next" [evar lexbuf]])
      (cases @ [Exp.case (Pat.any ()) (appfun "Sedlexing.backtrack" [evar lexbuf])])
  in
  let ret body = [ Vb.mk (pvar (state_fun i)) (func [pvar lexbuf, body]) ] in
  match best_final final with
    | None -> ret (body ())
    | Some _ when Array.length trans = 0 -> []
    | Some i -> ret (Exp.sequence (appfun "Sedlexing.mark" [evar lexbuf; int i]) (body ()))

let gen_definition lexbuf l error =
  let brs = Array.of_list l in
  let auto = Sedlex.compile (Array.map fst brs) in
  let cases = Array.to_list (Array.mapi (fun i (_, e) -> Exp.case (pint i) e) brs) in
  let states = Array.mapi (gen_state lexbuf auto) auto in
  let states = List.flatten (Array.to_list states) in
  Exp.let_ Recursive states
    (Exp.sequence
       (appfun "Sedlexing.start" [evar lexbuf])
       (Exp.match_ (appfun (state_fun 0) [evar lexbuf])
          (cases @ [Exp.case (Pat.any ()) error])
       )
    )

(* Representation of environment *)

type regex = Charset of Cset.t | Regex of Sedlex.regexp

let charset c = Charset c
let regex r = Regex r

let apply_rx f = function Charset c -> f (Sedlex.chars c) | Regex rx -> f rx

let apply_rx1 f x = regex (apply_rx f x)
let apply_rx2 f x y = apply_rx1 (apply_rx f x) y

let seq = apply_rx2 Sedlex.seq
let rep = apply_rx1 Sedlex.rep
let plus = apply_rx1 Sedlex.plus
let eps = regex Sedlex.eps
let alt re1 re2 = match re1, re2 with
  | Charset c1, Charset c2 -> charset (Cset.union c1 c2)
  | _, _ -> apply_rx2 Sedlex.alt re1 re2

let some_regex = apply_rx (fun x -> Some x)
let some_charset = function Charset c -> Some c | _ -> None

let regexp_of_regex = apply_rx (fun x -> x)

module StringMap = Map.Make(struct
  type t = string
  let compare = compare
end)

type env = regex StringMap.t

let env_lookup f name env =
  try f (StringMap.find name env) with _ -> None

let env_lookup_regex = env_lookup (fun x -> Some x)
let env_lookup_charset = env_lookup some_charset

(* Named regexps *)

let builtin_csets = [
  "any", Cset.any;
  "eof", Cset.eof;
  "xml_letter", Cset.letter;
  "xml_digit", Cset.digit;
  "xml_extender", Cset.extender;
  "xml_base_char", Cset.base_char;
  "xml_ideographic", Cset.ideographic;
  "xml_combining_char", Cset.combining_char;
  "xml_blank", Cset.blank;
  "tr8876_ident_char", Cset.tr8876_ident_char;

  (* Unicode 6.3 categories *)
  "cc", Unicode63.Categories.cc;
  "cf", Unicode63.Categories.cf;
  "cn", Unicode63.Categories.cn;
  "co", Unicode63.Categories.co;
  "cs", Unicode63.Categories.cs;
  "ll", Unicode63.Categories.ll;
  "lm", Unicode63.Categories.lm;
  "lo", Unicode63.Categories.lo;
  "lt", Unicode63.Categories.lt;
  "lu", Unicode63.Categories.lu;
  "mc", Unicode63.Categories.mc;
  "me", Unicode63.Categories.me;
  "mn", Unicode63.Categories.mn;
  "nd", Unicode63.Categories.nd;
  "nl", Unicode63.Categories.nl;
  "no", Unicode63.Categories.no;
  "pc", Unicode63.Categories.pc;
  "pd", Unicode63.Categories.pd;
  "pe", Unicode63.Categories.pe;
  "pf", Unicode63.Categories.pf;
  "pi", Unicode63.Categories.pi;
  "po", Unicode63.Categories.po;
  "ps", Unicode63.Categories.ps;
  "sc", Unicode63.Categories.sc;
  "sk", Unicode63.Categories.sk;
  "sm", Unicode63.Categories.sm;
  "so", Unicode63.Categories.so;
  "zl", Unicode63.Categories.zl;
  "zp", Unicode63.Categories.zp;
  "zs", Unicode63.Categories.zs;

  (* Unicode 6.3 properties *)
  "alphabetic", Unicode63.Properties.alphabetic;
  "ascii_hex_digit", Unicode63.Properties.ascii_hex_digit;
  "hex_digit", Unicode63.Properties.hex_digit;
  "id_continue", Unicode63.Properties.id_continue;
  "id_start", Unicode63.Properties.id_start;
  "lowercase", Unicode63.Properties.lowercase;
  "math", Unicode63.Properties.math;
  "other_alphabetic", Unicode63.Properties.other_alphabetic;
  "other_lowercase", Unicode63.Properties.other_lowercase;
  "other_math", Unicode63.Properties.other_math;
  "other_uppercase", Unicode63.Properties.other_uppercase;
  "uppercase", Unicode63.Properties.uppercase;
  "white_space", Unicode63.Properties.white_space;
  "xid_continue", Unicode63.Properties.xid_continue;
  "xid_start", Unicode63.Properties.xid_start;
]

let builtin_regexps =
  List.fold_left (fun rxs (n, c) -> StringMap.add n (charset c) rxs)
    StringMap.empty
    builtin_csets

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

let rec regexp_of_pattern env =
  let rec aux p =
    match p.ppat_desc with
    | Ppat_or (p1, p2) -> alt (aux p1) (aux p2)
    | Ppat_tuple (p :: pl) ->
        List.fold_left (fun r p -> seq r (aux p))
          (aux p)
          pl
    | Ppat_construct ({txt = Lident "Star"}, Some p) ->
        rep (aux p)
    | Ppat_construct ({txt = Lident "Plus"}, Some p) ->
        plus (aux p)
    | Ppat_construct ({txt = Lident "Opt"}, Some p) ->
        alt eps (aux p)
    | Ppat_constant (Const_string (s, _)) ->
        regex (regexp_for_string s)
    | Ppat_var {txt=x} ->
        begin match env_lookup_regex x env with
          | None -> err p.ppat_loc (Printf.sprintf "unbound regexp %s" x)
          | Some rx -> rx
        end
    | _ ->
        charset (cset_of_pattern env p)
  in
  aux

and cset_of_pattern env =
  let rec aux p =
    match p.ppat_desc with
    | Ppat_or (p1, p2) ->
        Cset.union (aux p1) (aux p2)
    | Ppat_construct ({txt = Lident "Compl"}, Some p) ->
        Cset.(difference any (aux p))
    | Ppat_construct ({txt = Lident "Chars"}, Some {ppat_desc=Ppat_constant (Const_string (s, _))}) ->
        let c = ref Cset.empty in
        for i = 0 to String.length s - 1 do
          c := Cset.union !c (Cset.singleton (Char.code s.[i]))
        done;
        !c
    | Ppat_interval (Const_char c1, Const_char c2) ->
        Cset.interval (Char.code c1) (Char.code c2)
    | Ppat_interval (Const_int i1, Const_int i2) ->
        Cset.interval (codepoint i1) (codepoint i2)
    | Ppat_constant (Const_char c) -> Cset.singleton (Char.code c)
    | Ppat_constant (Const_int c) -> Cset.singleton (codepoint c)
    | Ppat_var {txt=x} ->
        begin match env_lookup_charset x env with
          | None -> err p.ppat_loc (Printf.sprintf "unbound charset %s" x)
          | Some cs -> cs
        end
    | _ ->
        err p.ppat_loc "this pattern is not a valid regexp"
  in
  aux

let mapper =
  object(this)
    inherit Ast_mapper_class.mapper as super

    val env = builtin_regexps

    method define_regexp name p =
      {< env = StringMap.add name (regexp_of_pattern env p) env >}

    method! expr e =
      match e.pexp_desc with
      | Pexp_extension({txt="sedlex"}, PStr[{pstr_desc=Pstr_eval({pexp_desc=Pexp_match ({pexp_desc=Pexp_ident{txt=Lident lexbuf}}, cases)}, _)}]) ->
            let cases = List.rev cases in
            let error =
              match List.hd cases with
              | {pc_lhs = {ppat_desc = Ppat_any}; pc_rhs = e; pc_guard = None} ->
                  super # expr e
              | {pc_lhs = p} ->
                  err p.ppat_loc "the last branch must a catch-all error case"
            in
            let cases = List.rev (List.tl cases) in
            let cases =
              List.map
                (function
                  | {pc_lhs = p; pc_rhs = e; pc_guard = None} ->
                      regexp_of_regex (regexp_of_pattern env p), super # expr e
                  | {pc_guard = Some e} ->
                      err e.pexp_loc "'when' guards are not supported"
                ) cases
            in
            gen_definition lexbuf cases error
      | Pexp_let (Nonrecursive, [{pvb_pat={ppat_desc=Ppat_var{txt=name}}; pvb_expr={pexp_desc=Pexp_extension({txt="sedlex.regexp"}, PPat(p, None))}}], body) ->
          (this # define_regexp name p) # expr body
      | _ -> super # expr e


    val toplevel = true

    method! structure l =
      if toplevel then
        let l = {< toplevel = false >} # structure l in
        let parts = List.map partition (get_partitions ()) in
        let tables = List.map table (get_tables ()) in
        tables @ parts @ l
      else
        let mapper = ref this in
        List.concat
          (List.map
             (function
               | {pstr_desc = Pstr_value (Nonrecursive, [{pvb_pat={ppat_desc=Ppat_var{txt=name}}; pvb_expr={pexp_desc=Pexp_extension({txt="sedlex.regexp"}, PPat(p, None))}}])} ->
                 mapper := !mapper # define_regexp name p;
                 []
               | i ->
                 [ !mapper # structure_item i ]
             ) l)
  end


let () =
  Ast_mapper.register "sedlex" (fun _ -> Ast_mapper_class.to_mapper mapper)
