let rec token buf =
  match SEDLEX buf with
  | Plus xml_digit -> Printf.printf "Number %s\n" (Sedlexing.latin1_lexeme buf); token buf
  | xml_letter, Star (xml_digit | xml_letter) -> Printf.printf "Ident %s\n" (Sedlexing.latin1_lexeme buf); token buf
  | Plus xml_blank -> token buf
  | eof -> print_endline "EOF"

let () =
  let lexbuf = Sedlexing.from_latin1_string "foobar A123Bfoo  123Xbarfoo" in
(*
  let i = Sedlexing.next lexbuf in
  Printf.printf "%i\n%!" i;
*)
  token lexbuf
