let keywords : (string,unit) Hashtbl.t = Hashtbl.create 17

exception LocExn of int * int * exn		   
let error i j exn = raise (LocExn (i,j,exn))
exception Illegal_character of string
exception Unterminated_comment
exception Unterminated_string
exception Unterminated_string_in_comment


(* Buffer for string literals (always encoded in UTF8). *)
  
let string_buff = Buffer.create 1024

let store_lexeme lexbuf = 
  Buffer.add_string string_buff (Ulexing.utf8_lexeme lexbuf)
    
let store_ascii = Buffer.add_char string_buff
let store_char  = Buffer.add_string string_buff
let store_code  = Utf8.store string_buff
let get_stored_string () =
  let s = Buffer.contents string_buff in
  Buffer.clear string_buff;
  s
      
let string_start_pos = ref 0
let comment_start_pos = ref []

let parse_decimal_char s =
  int_of_string (String.sub s 1 (String.length s - 2))
    
let hexa_digit = function
  | '0'..'9' as c -> (Char.code c) - (Char.code '0')
  | 'a'..'f' as c -> (Char.code c) - (Char.code 'a') + 10
  | 'A'..'F' as c -> (Char.code c) - (Char.code 'A') + 10
  | _ -> failwith "Invalid hexadecimal digit" (* TODO: error loc *)
      
      
let parse_hexa_char s =
  let rec aux i accu =
    if i = String.length s - 1 then accu
    else aux (succ i) (accu * 16 + hexa_digit s.[i])
  in
  aux 2 0


let regexp ncname_char = 
  letter | digit | [ '.' '-' '_' ] | combining_char | extender
let regexp ncname = (letter | '_' ) ncname_char*
let regexp qname = (ncname ':')? ncname

module L = Ulexing

let rec token = lexer with
 | blank+ -> token lexbuf
 | qname ->
     let s = L.utf8_lexeme lexbuf in
     if Hashtbl.mem keywords s then "",s else "IDENT",s
 | ncname ":*" ->
     let s = L.utf8_lexeme_sub lexbuf 0 (L.lexeme_length lexbuf - 2) in
     "ANY_IN_NS", s
 | ".:*" -> 
     "ANY_IN_NS", ""
 | '-'? ['0'-'9']+ ->
     "INT", L.utf8_lexeme lexbuf
 | [ "<>=.,:;+-*/@&{}[]()|?`!" ]
 | "->" | "::" | ";;" | "--" | ":=" | "\\" | "++"
 | "{|" | "|}" | "<=" | ">=" | "<<" | ">>"
 | ["?+*"] "?" | "#" ->
     "", L.utf8_lexeme lexbuf
 | "#" ncname -> 
     "DIRECTIVE", L.utf8_lexeme lexbuf
 | '"' | "'" ->
     let double_quote = L.latin1_lexeme_char lexbuf 0 = '"' in
     string (L.lexeme_start lexbuf) double_quote lexbuf;
     (if double_quote then "STRING2" else "STRING1"), 
     (get_stored_string())

 | "(*" ->
     comment_start_pos := [L.lexeme_start lexbuf];
     comment lexbuf;
     token lexbuf
 | eof ->       
     "EOI",""
 | _ -> 
     error 
       (L.lexeme_start lexbuf) (L.lexeme_end lexbuf)
       (Illegal_character (L.utf8_lexeme lexbuf))

and comment = lexer with
  | "(*" ->
      comment_start_pos := L.lexeme_start lexbuf :: !comment_start_pos;
      comment lexbuf
  | "*)" ->
      comment_start_pos := List.tl !comment_start_pos;
      if !comment_start_pos <> [] then comment lexbuf;
  | eof ->
      let st = List.hd !comment_start_pos in
      error st (st+2) Unterminated_comment
  | _ ->
      comment lexbuf

and string start double = lexer with
  | '"' | "'" ->
      let d = L.latin1_lexeme_char lexbuf 0 = '"' in
      if d != double then (store_lexeme lexbuf; string start double lexbuf)
  | '\\' ['\\' '"' '\''] ->
      store_ascii (L.latin1_lexeme_char lexbuf 1);
      string start double lexbuf
  | "\\n" -> 
      store_ascii '\n';	string start double lexbuf
  | "\\t" -> 
      store_ascii '\t';	string start double lexbuf
  | "\\r" -> 
      store_ascii '\r';	string start double lexbuf
  | '\\' ['0'-'9']+ ';' ->
      store_code (parse_decimal_char (L.utf8_lexeme lexbuf));
      string start double lexbuf
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F']+ ';' ->
      store_code (parse_hexa_char (L.utf8_lexeme lexbuf));
      string start double lexbuf
  | '\\' ->
      error 
        (L.lexeme_start lexbuf) (L.lexeme_end lexbuf)
        (Illegal_character (L.utf8_lexeme lexbuf))
  | eof ->
      error start (start + 1) Unterminated_string
  | _ ->
      store_lexeme lexbuf;
      string start double lexbuf


let () =
  let lexbuf = Ulexing.from_latin1_file "../cduce/web/site.cd" in
  try
    while true do
      let (a,b) = token lexbuf in
      Printf.printf "%s: \"%s\"\n" a b;
      if a = "EOI" then exit 0
    done
  with Ulexing.Error -> 
    Printf.eprintf "Lexing error at offset %i\n" (Ulexing.lexeme_start lexbuf)
