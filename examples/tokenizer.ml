let '0'..'9' as digit = SEDLEX.regexp
let (Plus digit) as number = SEDLEX.regexp

let rec token buf =
  let ('a'..'z'|'A'..'Z') as letter = SEDLEX.regexp in
  match SEDLEX buf with
  | number -> Printf.printf "Number %s\n" (Sedlexing.Latin1.lexeme buf); token buf
  | letter, Star ('A'..'Z' | 'a'..'z' | digit) -> Printf.printf "Ident %s\n" (Sedlexing.Latin1.lexeme buf); token buf
  | Plus xml_blank -> token buf
  | Plus (Chars "+*-/") -> Printf.printf "Op %s\n" (Sedlexing.Latin1.lexeme buf); token buf
  | 128 .. 255 -> print_endline "Non ASCII"
  | eof -> print_endline "EOF"
  | _ -> failwith "Unexpected character"

let () =
  let lexbuf = Sedlexing.Latin1.from_string "foobar A123Bfoo  ++123Xbar/foo" in
  token lexbuf
