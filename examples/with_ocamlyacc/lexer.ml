(* Sedlex lexer that produces tokens for an ocamlyacc parser.

   The key integration point: ocamlyacc parsers expect a function of type
   [Lexing.lexbuf -> token], but sedlex uses [Sedlexing.lexbuf].
   We create a dummy [Lexing.lexbuf] and update its position fields
   after each token so that error reporting works correctly. *)

let digit = [%sedlex.regexp? '0' .. '9']

let rec token buf =
  match%sedlex buf with
    | Plus digit -> Parser.INT (int_of_string (Sedlexing.Utf8.lexeme buf))
    | '+' -> Parser.PLUS
    | '-' -> Parser.MINUS
    | '*' -> Parser.TIMES
    | '/' -> Parser.DIV
    | '(' -> Parser.LPAREN
    | ')' -> Parser.RPAREN
    | Plus white_space -> token buf
    | eof -> Parser.EOF
    | _ -> failwith ("Unexpected character: " ^ Sedlexing.Utf8.lexeme buf)

(* Wrap a sedlex lexer for use with ocamlyacc.
   Returns [(tokenize, lexing_lexbuf)] where [tokenize] has type
   [Lexing.lexbuf -> token] as expected by the generated parser. *)
let tokenize buf =
  let lexing_lexbuf = Lexing.from_string "" in
  let tokenize _lexing_lexbuf =
    let tok = token buf in
    let start_pos, end_pos = Sedlexing.lexing_positions buf in
    _lexing_lexbuf.Lexing.lex_start_p <- start_pos;
    _lexing_lexbuf.Lexing.lex_curr_p <- end_pos;
    tok
  in
  (tokenize, lexing_lexbuf)
