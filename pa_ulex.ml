let char s = Char.code (Token.eval_char s)

let named_regexps =
  (Hashtbl.create 13 : (string, Ulex.regexp) Hashtbl.t)

let regexp_for_string s =
  let rec aux n =
    if n = String.length s then Ulex.eps
    else Ulex.seq (Ulex.chars (Cset.singleton (Char.code s.[n]))) (aux (succ n))
  in aux 0

let () =
  Hashtbl.add named_regexps "eof" (Ulex.chars Cset.eof)

let let_regexp = Grammar.Entry.create Pcaml.gram "pa_ulex let"
let header = Grammar.Entry.create Pcaml.gram "pa_ulex header"
let lexer_def = Grammar.Entry.create Pcaml.gram "pa_ulex lexerdef"

let partition (i,p) =
  let loc = (-1,-1) in
  let body = 
    List.fold_left 
      (fun e (a,b,i) ->
	 <:expr< if 
	   (c >= $int: string_of_int a$) &&
	   (c <= $int: string_of_int b$) then $int: string_of_int i$
	 else $e$ >>) <:expr< -1 >> p in
  let f = Printf.sprintf "ulex_partition_%i" i in
  let l1 = <:str_item< value $lid:f$ = fun c -> $body$ >> in
  let l2 = <:str_item< declare $list:[]$ end >> in
  <:str_item< declare $list:[l1;l2]$ end >>, loc


EXTEND
 GLOBAL: Pcaml.str_item let_regexp header lexer_def;

 let_regexp: [
   [ x = LIDENT; "="; r = regexp ->
       if Hashtbl.mem named_regexps x then
         Printf.eprintf 
           "pa_ulex (warning): multiple definition of named regexp '%s'\n"
           x;
       Hashtbl.add named_regexps x r;
   ]
 ];

 lexer_def: [
   [ def = LIST0 definition SEP "and" ->
       let output = [] in
       <:str_item< declare $list: output$ end >> 
   ]
 ];

 Pcaml.expr: [
  [ "lexer"; e = definition -> e ]
 ];

 Pcaml.str_item: [
   [ "let"; LIDENT "regexp"; let_regexp -> 
       <:str_item< declare $list: []$ end >>
   ]
 ];
 
 definition: [
   [ pl = LIST0 Pcaml.patt LEVEL "simple"; "with";
     OPT "|"; l = LIST0 [ r=regexp; "->"; a=Pcaml.expr -> (r,a) ] SEP "|" ->
       let brs = Array.of_list l in
       let rs = Array.map fst brs in
       let auto = Ulex.compile rs in
       let gen_state i (part,trans,final) =
	 let f = Printf.sprintf "state_%i" i in
	 let p = Printf.sprintf "ulex_partition_%i" part in
	 let cases =
	   Array.mapi 
	     (fun i j ->
		let f = Printf.sprintf "state_%i" j in
		<:patt< $int:string_of_int i$ >>,
		None,
		<:expr< $lid:f$ lexbuf >>
	     ) trans in
	 let cases = Array.to_list cases in
	 let cases = cases @ [<:patt< _ >>, None, <:expr< Ulexing.backtrack lexbuf >>] in
	 let body = 
	   <:expr< match ($lid:p$ (Ulexing.next lexbuf)) 
	           with [ $list:cases$ ] >> in
 	 let fin = ref None in
	 Array.iteri 
	   (fun i b -> if b && (!fin = None) then fin := Some i) final;
	 let body = match !fin with
	   | None -> body
	   | Some i -> 
	       if Array.length trans = 0 then
		 <:expr< $int:string_of_int i$ >>
	       else
		 <:expr< 
                   do { Ulexing.mark lexbuf $int:string_of_int i$; 
			$body$ } >> in
	 <:patt< $lid:f$ >>, <:expr< fun lexbuf -> $body$ >>
      in

       let states = Array.to_list (Array.mapi gen_state auto) in
       let states = <:expr< let rec $list:states$ in () >> in

       let body = states in
       let abstr = 
	 List.fold_right 
	   (fun p e -> <:expr< fun $p$ -> $e$>>) pl body in
       <:expr< fun lexbuf -> $abstr$ >>
   ]
 ];

 header:  [
   [ "{"; e = LIST0 [ si = Pcaml.str_item; OPT ";;" -> si ]; "}" -> 
       [<:str_item< declare $list:e$ end>>, loc] ]
   | [ -> [] ]
 ];

 regexp: [
   [ r1 = regexp; "|"; r2 = regexp -> Ulex.alt r1 r2 ]
 | [ r1 = regexp; r2 = regexp -> Ulex.seq r1 r2 ]
 | [ r = regexp; "*" -> Ulex.rep r
   | r = regexp; "+" -> Ulex.seq (Ulex.rep r) r
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
   ]
 ];
END


let () =
  let old_parse_implem = !Pcaml.parse_implem in
  let new_parse_implem s =
    let (items,d) = old_parse_implem s in
    let parts = List.map partition (Ulex.partitions ()) in
    parts @ items, d
  in
  Pcaml.parse_implem := new_parse_implem

