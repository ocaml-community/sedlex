let () =
  let input = "1 + 2 * (3 - 1)" in

  (* Classic API *)
  let buf = Sedlexing.Utf8.from_string input in
  let tokenize, lexbuf = Lexer.tokenize buf in
  let result = Parser.main tokenize lexbuf in
  Printf.printf "classic:     %s = %d\n" input result;

  (* Incremental API *)
  let buf = Sedlexing.Utf8.from_string input in
  let supplier = Lexer.tokenize_incremental buf in
  let result =
    Parser.MenhirInterpreter.loop supplier
      (Parser.Incremental.main Lexing.dummy_pos)
  in
  Printf.printf "incremental: %s = %d\n" input result
