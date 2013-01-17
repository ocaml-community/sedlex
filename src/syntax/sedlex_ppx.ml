open Longident
open Parsetree
open Location
open Asttypes
open Ast_mapper

(* Named regexps *)

let named_regexps : (string, Sedlex.regexp) Hashtbl.t = Hashtbl.create 13

(* Predefined regexps *)

let () =
  List.iter (fun (n,c) -> Hashtbl.add named_regexps n (Sedlex.chars c))
    [
      "eof", Cset.eof;
      "xml_letter", Cset.letter;
      "xml_digit", Cset.digit;
      "xml_extender", Cset.extender;
      "xml_base_char", Cset.base_char;
      "xml_ideographic", Cset.ideographic;
      "xml_combining_char", Cset.combining_char;
      "xml_blank", Cset.blank;

      "tr8876_ident_char", Cset.tr8876_ident_char;
    ]

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

let decision_table p =
  simplify (-1) (Cset.max_code) (decision_table p)

let tables : (int array, string) Hashtbl.t = Hashtbl.create 31
let tables_counter = ref 0
let get_tables () =
  let t = Hashtbl.fold (fun key x accu -> (x, key) :: accu) tables [] in
  Hashtbl.clear tables;
  t

let table_name t =
  try Hashtbl.find tables t
  with Not_found ->
    incr tables_counter;
    let n = Printf.sprintf "__sedlex_table_%i" !tables_counter in
    Hashtbl.add tables t n;
    n

let output_byte buf b =
  Buffer.add_char buf '\\';
  Buffer.add_char buf (Char.chr(48 + b / 100));
  Buffer.add_char buf (Char.chr(48 + (b / 10) mod 10));
  Buffer.add_char buf (Char.chr(48 + b mod 10))

let output_byte_array v =
  let n = Array.length v in
  let s = String.create n in
  for i = 0 to n - 1 do s.[i] <- Char.chr v.(i) done;
  E.constant (Const_string s)

let eid s = E.ident (mknoloc (Longident.parse s))
let appfun s l = E.apply_nolabs (eid s) l
let int i = E.constant (Const_int i)
let pint i = P.constant (Const_int i)
let pvar name = P.var (Location.mknoloc name)
let glb_value name def = M.value Nonrecursive [pvar name, def]
let fun_ arg body = E.function_ "" None [pvar arg, body]

let table (n, t) =
  glb_value n (output_byte_array t)

let partition_name i = Printf.sprintf "__sedlex_partition_%i" i

let partition (i, p) =
  let rec gen_tree = function
    | Lte (i, yes, no) ->
        E.ifthenelse
            (appfun "<=" [eid "c"; int i])
            (gen_tree yes)
            (Some (gen_tree no))
    | Return i -> int i
    | Table (offset, t) ->
	let c =
          if offset = 0 then eid "c"
	  else appfun "-" [eid "c"; int offset]
        in
        appfun "-"
          [
           appfun "Char.code" [appfun "String.get" [eid (table_name t); c]];
           int 1;
          ]
  in
  let body = gen_tree (decision_table p) in
  glb_value (partition_name i) (fun_ "c" body)

(* Code generation for the automata *)

let best_final final =
  let fin = ref None in
  for i = Array.length final - 1 downto 0 do
    if final.(i) then fin := Some i
  done;
  !fin

let state_fun state = Printf.sprintf "__sedlex_state_%i" state

let call_state lexbuf auto state =
  let (_, trans, final) = auto.(state) in
  if Array.length trans = 0
  then match best_final final with
  | Some i -> int i
  | None -> assert false
  else appfun (state_fun state) [eid lexbuf]

let gen_state lexbuf auto i (part, trans, final) =
  let cases = Array.mapi (fun i j -> (pint i, call_state lexbuf auto j)) trans in
  let cases = Array.to_list cases in
  let body =
    E.match_
      (appfun (partition_name part) [appfun "Sedlexing.next" [eid lexbuf]])
      (cases @ [P.any (), appfun "Sedlexing.backtrack" [eid lexbuf]])
  in
  let ret body = [ pvar (state_fun i), (fun_ lexbuf body) ] in
  match best_final final with
    | None -> ret body
    | Some _ when Array.length trans = 0 -> []
    | Some i -> ret (E.sequence (appfun "Sedlexing.mark" [eid lexbuf; int i]) body)

let gen_definition lexbuf l =
  let brs = Array.of_list l in
  let rs = Array.map fst brs in
  let auto = Sedlex.compile rs in

  let cases = Array.to_list (Array.mapi (fun i (_,e) -> (pint i, e)) brs) in
  let states = Array.mapi (gen_state lexbuf auto) auto in
  let states = List.flatten (Array.to_list states) in
  E.let_ Recursive states
    (E.sequence
       (appfun "Sedlexing.start" [eid lexbuf])
       (E.match_ (appfun (state_fun 0) [eid lexbuf])
          (cases @ [P.any (), appfun "Sedlexing.error" [eid lexbuf]])
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

let rec regexp_of_pattern p =
  match p.ppat_desc with
  | Ppat_or (p1, p2) -> Sedlex.alt (regexp_of_pattern p1) (regexp_of_pattern p2)
  | Ppat_tuple (p :: pl) ->
      List.fold_left (fun r p -> Sedlex.seq r (regexp_of_pattern p))
        (regexp_of_pattern p)
        pl
  | Ppat_construct ({txt = Lident "Star"}, Some p, _) ->
      Sedlex.rep (regexp_of_pattern p)
  | Ppat_construct ({txt = Lident "Plus"}, Some p, _) ->
      Sedlex.plus (regexp_of_pattern p)
  | Ppat_construct ({txt = Lident "Opt"}, Some p, _) ->
      Sedlex.alt Sedlex.eps (regexp_of_pattern p)
  | Ppat_any -> Sedlex.chars Cset.any
  | Ppat_constant (Const_string s) -> regexp_for_string s
  | Ppat_constant (Const_char c) -> regexp_for_char c
  | Ppat_constant (Const_int c) -> Sedlex.chars (Cset.singleton (codepoint c))
  | Ppat_var {txt=x} ->
      begin try Hashtbl.find named_regexps x
      with Not_found ->
        Format.eprintf "%aSedlex: unbound regexp %s.@."
          Location.print p.ppat_loc
          x;
        exit 2
      end
  | _ ->
      Format.eprintf "%aSedlex: this pattern is not a valid regexp.@."
        Location.print p.ppat_loc;
      exit 2
(* TODO: classes, named regexps *)




let mapper =
  object
    inherit Ast_mapper.mapper as super

    method! expr e =
      match e.pexp_desc with
      | Pexp_match
          ({pexp_desc=Pexp_construct ({txt=Lident "SEDLEX"}, Some {pexp_desc=Pexp_ident{txt=Lident lexbuf}}, _)}, cases) ->
            let cases = List.map (fun (p, e) -> regexp_of_pattern p, super # expr e) cases in
            gen_definition lexbuf cases
      | _ -> super # expr e

    method! implementation file str =
      let file, str = super # implementation file str in
      let parts = List.map partition (Sedlex.partitions ()) in
      let tables = List.map table (get_tables ()) in
      file, tables @ parts @ str

  end

let () = Ast_mapper.main mapper

(*

EXTEND Gram
 GLOBAL: expr str_item;

 expr: [
  [ "lexer";
     OPT "|"; l = LIST0 [ r=regexp; "->"; a=expr -> (r,a) ] SEP "|" ->
       gen_definition _loc l ]
 ];

 str_item: [
   [ "let"; LIDENT "regexp"; x = LIDENT; "="; r = regexp ->
       if Hashtbl.mem named_regexps x then
         Printf.eprintf
           "pa_sedlex (warning): multiple definition of named regexp '%s'\n"
           x;
     Hashtbl.add named_regexps x r;
       <:str_item<>>
   ]
 ];

 regexp: [
   [ r1 = regexp; "|"; r2 = regexp -> Sedlex.alt r1 r2 ]
 | [ r1 = regexp; r2 = regexp -> Sedlex.seq r1 r2 ]
 | [ r = regexp; "*" -> Sedlex.rep r
   | r = regexp; "+" -> Sedlex.plus r
   | r = regexp; "?" -> Sedlex.alt Sedlex.eps r
   | "("; r = regexp; ")" -> r
   | "_" -> Sedlex.chars Cset.any
   | c = chr -> Sedlex.chars (Cset.singleton c)
   | `STRING (s,_) -> regexp_for_string s
   | "["; cc = ch_class; "]" -> Sedlex.chars cc
   | "[^"; cc = ch_class; "]" -> Sedlex.chars (Cset.difference Cset.any cc)
   | x = LIDENT ->
       try  Hashtbl.find named_regexps x
       with Not_found ->
         failwith
           ("pa_sedlex (error): reference to unbound regexp name `"^x^"'")
   ]
 ];

 chr: [
   [ `CHAR (c,_) -> Char.code c
   | i = INT -> codepoint i ]
 ];

 ch_class: [
   [ c1 = chr; "-"; c2 = chr -> Cset.interval c1 c2
   | c = chr -> Cset.singleton c
   | cc1 = ch_class; cc2 = ch_class -> Cset.union cc1 cc2
   | `STRING (s,_) ->
       let c = ref Cset.empty in
       for i = 0 to String.length s - 1 do
	 c := Cset.union !c (Cset.singleton (Char.code s.[i]))
       done;
       !c
   ]
 ];
END



let change_ids suffix = object
  inherit Ast.map as super
  method ident = function
    | Ast.IdLid (loc, s) when String.length s > 6 && String.sub s 0 6 = "__sedlex" -> Ast.IdLid (loc, s ^ suffix)
    | i -> i
end

let () =
  let first = ref true in
  AstFilters.register_str_item_filter
    (fun s ->
       assert(!first); first := false;
       let parts = List.map partition (Sedlex.partitions ()) in
       let tables = List.map table (get_tables ()) in
       let suffix = "__" ^ Digest.to_hex (Digest.string (Marshal.to_string (parts, tables) [])) in
       (change_ids suffix) # str_item <:str_item< $list:tables$; $list:parts$; $s$ >>
    )
*)
