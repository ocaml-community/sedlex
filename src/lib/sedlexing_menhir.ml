open Sedlexing

exception ParseError of (string * int * int)

let current_position ?(file="") lexbuf =
  let (line, bol, pos) = pos lexbuf in
  let col = pos - bol in
  (file, line, col)

let current_lexing_position ?(file="") lexbuf =
  let (line, bol, pos) = pos lexbuf in
  let open Lexing in
  {
    pos_fname = file;
    pos_lnum = line;
    pos_bol = bol;
    pos_cnum = pos;
  }

let raise_ParseError ?(file="") lexbuf =
  raise @@ ParseError (current_position ~file lexbuf)

let string_of_ParseError (file, line, cnum) =
  let file_to_string file =
    if file = "" then ""
    else " on file " ^ file
  in
  Printf.sprintf
    "Parse error%s line %i, column %i"
    (file_to_string file)
    line cnum

let sedlex_with_menhir ?(file="") parser' lexer' lexbuf =
  let lexer () =
    let ante_position = current_lexing_position ~file lexbuf in
    let token = lexer' lexbuf in
    let post_position = current_lexing_position ~file lexbuf
    in (token, ante_position, post_position) in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised parser'
  in
  try
    parser lexer
  with
    | Sedlexing.MalFormed
    | Sedlexing.InvalidCodepoint _
      -> raise_ParseError ~file lexbuf