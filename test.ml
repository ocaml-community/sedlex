let regexp number = [ '0'-'9' ]+

let token def = lexer
  | letter+ -> Printf.sprintf "word(%s)" (Ulexing.utf8_lexeme lexbuf)
  | number -> "number"
  | eof -> exit 0
  | [1234-1246] -> "bla"
  | _ -> def


let () =
  let lexbuf = Ulexing.from_latin1_string "abcABC123!+xyzéé" in
  try
    while true do
      let r = token "???" lexbuf in
      Printf.printf "%s\n" r
    done
  with Ulexing.Error -> 
    Printf.eprintf "Lexing error at offset %i\n" (Ulexing.lexeme_start lexbuf)
