(* Sedlex lexer that produces tokens for a menhir parser.

   Two approaches are shown:

   1. [tokenize] — for menhir's classic API (same technique as ocamlyacc):
      wraps the sedlex lexer into [Lexing.lexbuf -> token].

   2. [tokenize_incremental] — for menhir's incremental API:
      uses [Sedlexing.with_tokenizer] which returns a
      [unit -> token * position * position] supplier. *)

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

(* Approach 1: classic API — wrap into [Lexing.lexbuf -> token] *)
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

(* Approach 2: incremental API — use [Sedlexing.with_tokenizer] *)
let tokenize_incremental buf = Sedlexing.with_tokenizer token buf
