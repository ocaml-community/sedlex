let '0'..'9' as digit = SEDLEX.regexp
let (Plus digit) as number = SEDLEX.regexp
let ('a'..'z'|'A'..'Z') as letter = SEDLEX.regexp

let rec token buf =
  match SEDLEX buf with
  | number -> Printf.printf "Number %s\n" (Sedlexing.latin1_lexeme buf); token buf
  | letter, Star ('A'..'Z' | 'a'..'z' | digit) -> Printf.printf "Ident %s\n" (Sedlexing.latin1_lexeme buf); token buf
  | Plus xml_blank -> token buf
  | Plus (Chars "+*-/") -> Printf.printf "Op %s\n" (Sedlexing.latin1_lexeme buf); token buf
  | Range(128,255) -> print_endline "Non ASCII"
  | eof -> print_endline "EOF"

let () =
  let lexbuf = Sedlexing.from_latin1_string "foobar A123Bfoo  ++123Xbar/foo" in
  token lexbuf
