let x = 1
let regexp letter = [ 'a'-'z' ]
let regexp number = [ '0'-'9' ]+

let rec token = lexer with
  | letter+ -> "word:" ^ (Ulexing.latin1_lexeme lexbuf)
  | number -> "number"
  | _ -> Printf.sprintf "another char (codepoint=%c)" (Ulexing.latin1_lexeme_char lexbuf 0)
  | eof -> exit 0


let () =
  let lexbuf = Ulexing.from_latin1 "abc123@@def" in
  try
    while true do
      let r = token lexbuf in
      Printf.printf "%s\n" r
    done
  with Ulexing.Error -> Printf.eprintf "Lexing error at offset %i\n" (Ulexing.lexeme_start lexbuf)
