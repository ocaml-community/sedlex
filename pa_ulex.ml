let loc = (-1,-1)

(* Named regexp *)

let named_regexps =
  (Hashtbl.create 13 : (string, Ulex.regexp) Hashtbl.t)

let () =
  List.iter (fun (n,c) -> Hashtbl.add named_regexps n (Ulex.chars c))
    [
      "eof", Cset.eof;
      "letter", Cset.letter;
      "digit", Cset.digit;
      "extender", Cset.extender;
      "base_char", Cset.base_char;
      "ideographic", Cset.ideographic;
      "combining_char", Cset.combining_char;
      "blank", Cset.blank
    ] 

(* Decision tree for partitions *)

type decision_tree =
  | Lte of int * decision_tree * decision_tree
  | Table of int * int array
  | Return of int

let decision l =
  let l = List.map (fun (a,b,i) -> (a,b,Return i)) l in
  let rec merge2 = function
    | (a1,b1,d1) :: (a2,b2,d2) :: rest ->
	let x =
	  if b1 + 1 = a2 then d2
	  else Lte (a2 - 1,Return (-1), d2)
	in
	(a1,b2, Lte (b1,d1, x)) :: (merge2 rest)
    | rest -> rest in
  let rec aux = function
    | _::_::_ as l -> aux (merge2 l)
    | [(a,b,d)] -> Lte (a - 1, Return (-1), Lte (b, d, Return (-1)))
    | _ -> Return (-1)
  in
  aux l

let limit = 256

let decision_table l =
  let rec aux m accu = function
    | ((a,b,i) as x)::rem when (b < limit && i < 255)-> 
	aux (min a m) (x::accu) rem
    | rem -> m,accu,rem in
  match (aux max_int [] l : int * 'a list * 'b list) with
    | _,[], _ -> decision l
    | min,((_,max,_)::_ as l1), l2 ->
	let arr = Array.create (max-min+1) 0 in
	List.iter (fun (a,b,i) -> for j = a to b do arr.(j-min) <- i + 1 done) l1;
	Lte (min-1, Return (-1), Lte (max, Table (min,arr), decision l2))

let rec simplify min max = function
  | Lte (i,yes,no) ->
      if i >= max then simplify min max yes 
      else if i < min then simplify min max no
      else Lte (i, simplify min i yes, simplify (i+1) max no)
  | x -> x
		   

let tables = Hashtbl.create 31
let tables_counter = ref 0
let get_tables () = 
  let t = Hashtbl.fold (fun key x accu -> (x,key)::accu) tables [] in
  Hashtbl.clear tables;
  t
let table_name t =
  try Hashtbl.find tables t
  with Not_found ->
    incr tables_counter;
    let n = Printf.sprintf "__ulex_table_%i" !tables_counter in
    Hashtbl.add tables t n;
    n

let output_byte buf b =
  Buffer.add_char buf '\\';
  Buffer.add_char buf (Char.chr(48 + b / 100));
  Buffer.add_char buf (Char.chr(48 + (b / 10) mod 10));
  Buffer.add_char buf (Char.chr(48 + b mod 10))

let output_byte_array v =
  let b = Buffer.create (Array.length v * 5) in
  for i = 0 to Array.length v - 1 do
    output_byte b (v.(i) land 0xFF);
    if i land 15 = 15 then Buffer.add_string b "\\\n    "
  done;
  let s = Buffer.contents b in
  <:expr< $str:s$ >>

let table (n,t) = <:str_item< value $lid:n$ = $output_byte_array t$ >>

let partition_name i = Printf.sprintf "__ulex_partition_%i" i

let partition (i,p) =
  let rec gen_tree = function 
    | Lte (i,yes,no) ->
	<:expr< if (c <= $int: string_of_int i$) 
	then $gen_tree yes$ else $gen_tree no$ >>
    | Return i ->
	<:expr< $int: string_of_int i$ >>
    | Table (offset, t) ->
	let c = if offset = 0 then <:expr< c >> 
	else <:expr< (c - $int: string_of_int offset$) >> in
	<:expr< Char.code ($lid: table_name t$.[$c$]) - 1>>
  in
  let body = gen_tree (simplify (-1) (Cset.max_code) (decision_table p)) in
  let f = partition_name i in
  <:str_item< value $lid:f$ = fun c -> $body$ >>


(* Code generation for the automata *)

let best_final final =
  let fin = ref None in
  Array.iteri 
    (fun i b -> if b && (!fin = None) then fin := Some i) final;
  !fin

let call_state auto state =
  match auto.(state) with (_,trans,final) ->
    if Array.length trans = 0 
    then match best_final final with
      | Some i -> <:expr< $int:string_of_int i$ >>
      | None -> assert false
    else
      let f = Printf.sprintf "__ulex_state_%i" state in
      <:expr< $lid:f$ lexbuf >>
	

let gen_state auto loc i (part,trans,final) = 
  let f = Printf.sprintf "__ulex_state_%i" i in
  let p = partition_name part in
  let cases =
    Array.mapi 
      (fun i j ->
	 <:patt< $int:string_of_int i$ >>,
	 None,
	 call_state auto j
      ) trans in
  let cases = Array.to_list cases in
  let cases = cases @ [<:patt< _ >>, None, <:expr< Ulexing.backtrack lexbuf >>] in
  let body = 
    <:expr< match ($lid:p$ (Ulexing.next lexbuf)) 
    with [ $list:cases$ ] >> in
  let ret body =
    [<:patt< $lid:f$ >>, <:expr< fun lexbuf -> $body$ >>] in
  match best_final final with
    | None -> ret body
    | Some i -> 
	if Array.length trans = 0 then [] else
	  ret
	  <:expr< do { Ulexing.mark lexbuf $int:string_of_int i$;  $body$ } >>


let gen_definition loc l =
  let brs = Array.of_list l in
  let rs = Array.map fst brs in
  let auto = Ulex.compile rs in

  let cases = Array.mapi (fun i (_,e) -> <:patt< $int:string_of_int i$ >>, None, e) brs in
  let cases = Array.to_list cases in
  let cases = cases @ [<:patt< _ >>, None, <:expr< raise Ulexing.Error >>] in
  let actions = <:expr< match __ulex_state_0 lexbuf with [ $list:cases$ ] >> in
  let states = Array.mapi (gen_state auto loc) auto in
  let states = List.flatten (Array.to_list states) in
  let body = <:expr< let rec $list:states$ in do { Ulexing.start lexbuf; $actions$ } >> in
  <:expr< fun lexbuf -> $body$ >>


(* Lexer specification parser *)

let char s = Char.code (Token.eval_char s)

let regexp_for_string s =
  let rec aux n =
    if n = String.length s then Ulex.eps
    else 
      Ulex.seq (Ulex.chars (Cset.singleton (Char.code s.[n]))) (aux (succ n))
  in aux 0

EXTEND
 GLOBAL: Pcaml.expr Pcaml.str_item;

 Pcaml.expr: [
  [ "lexer"; "with";
     OPT "|"; l = LIST0 [ r=regexp; "->"; a=Pcaml.expr -> (r,a) ] SEP "|" ->
       gen_definition loc l ]
 ];

 Pcaml.str_item: [
   [ "let"; LIDENT "regexp"; x = LIDENT; "="; r = regexp ->
       if Hashtbl.mem named_regexps x then
         Printf.eprintf 
           "pa_ulex (warning): multiple definition of named regexp '%s'\n"
           x;
     Hashtbl.add named_regexps x r;
       <:str_item< declare $list: []$ end >>
   ]
 ];
 
 regexp: [
   [ r1 = regexp; "|"; r2 = regexp -> Ulex.alt r1 r2 ]
 | [ r1 = regexp; r2 = regexp -> Ulex.seq r1 r2 ]
 | [ r = regexp; "*" -> Ulex.rep r
   | r = regexp; "+" -> Ulex.plus r
   | r = regexp; "?" -> Ulex.alt Ulex.eps r
   | "("; r = regexp; ")" -> r
   | "_" -> Ulex.chars Cset.any
   | c = CHAR -> Ulex.chars (Cset.singleton (char c))
   | s = STRING -> regexp_for_string (Token.eval_string s)
   | "["; cc = ch_class; "]" ->  Ulex.chars cc
   | x = LIDENT ->
       try  Hashtbl.find named_regexps x
       with Not_found ->
         failwith 
           ("pa_ulex (error): reference to unbound regexp name `"^x^"'")
   ]
 ];

 ch_class: [
   [ "^"; cc = ch_class -> Cset.complement cc]
 | [ c1 = CHAR; "-"; c2 = CHAR -> Cset.interval (char c1) (char c2)
   | c = CHAR -> Cset.singleton (char c)
   | cc1 = ch_class; cc2 = ch_class -> Cset.union cc1 cc2
   | s = STRING -> 
       let s = Token.eval_string s in
       let c = ref Cset.empty in
       for i = 0 to String.length s - 1 do
	 c := Cset.union !c (Cset.singleton (Char.code s.[i])) 
       done;
       !c
   ]
 ];
END


let () =
  let old_parse_implem = !Pcaml.parse_implem in
  let new_parse_implem s =
    let (items,d) = old_parse_implem s in
    let parts = List.map partition (Ulex.partitions ()) in
    let tables = List.map table (get_tables ()) in
    (<:str_item< declare $list:tables@parts$ end >>, loc) :: items, d
  in
  Pcaml.parse_implem := new_parse_implem

