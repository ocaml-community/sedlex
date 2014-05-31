
let uppercase_or_lowercase = [%sedlex.regexp? uppercase | lowercase]

let rec token buf =
  match%sedlex buf with
  | lowercase -> print_endline "Lowercase!"; token buf
  | Compl uppercase_or_lowercase -> print_endline "Not lowercase OR uppercase!"; token buf
  | uppercase -> print_endline "Uppercase!"; token buf
  | eof -> print_endline "EOF"
  | _ -> assert false

let () =
  let lexbuf = Sedlexing.Latin1.from_string "Abc::DefG" in
  token lexbuf
