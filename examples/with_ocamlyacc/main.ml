let () =
  let input = "1 + 2 * (3 - 1)" in
  let buf = Sedlexing.Utf8.from_string input in
  let tokenize, lexbuf = Lexer.tokenize buf in
  let result = Parser.main tokenize lexbuf in
  Printf.printf "%s = %d\n" input result
