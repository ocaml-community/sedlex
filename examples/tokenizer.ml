let rec token buf =
  match SEDLEX buf with
  | Plus ('0'..'9') -> Printf.printf "Number %s\n" (Sedlexing.latin1_lexeme buf); token buf
  | ('a'..'z'|'A'..'Z'), Star ('A'..'Z' | 'a'..'z' | '0'..'9') -> Printf.printf "Ident %s\n" (Sedlexing.latin1_lexeme buf); token buf
  | Plus xml_blank -> token buf
  | Plus (Chars "+*-/") -> Printf.printf "Op %s\n" (Sedlexing.latin1_lexeme buf); token buf
  | Range(128,255) -> print_endline "Non ASCII"
  | eof -> print_endline "EOF"

let () =
  let lexbuf = Sedlexing.from_latin1_string "foobar A123Bfoo  ++123Xbar/foo" in
  token lexbuf
