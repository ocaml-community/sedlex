
let rec token buf =
  match%sedlex buf with
  | ',' -> print_endline "Comma!"; token buf
  | Compl ',' -> print_endline "Not comma!"; token buf
  | eof -> print_endline "EOF"
  | _ -> assert false

let () =
  let lexbuf = Sedlexing.Latin1.from_string ",',':',," in
  token lexbuf
