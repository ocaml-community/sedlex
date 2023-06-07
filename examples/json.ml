open Sedlexing

type token =
  | EOF
  | LBRACKET
  | RBRACKET
  | COLON
  | COMMA
  | MINUS
  | PLUS
  | LSQUARE
  | RSQUARE
  | STRING of string
  | BOOL of bool
  | NUMBER of string

exception LexError of Lexing.position * string

let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]
let blank = [%sedlex.regexp? ' ' | '\t']
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]
let any_blank = [%sedlex.regexp? blank | newline]
let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let decimal_ascii = [%sedlex.regexp? Plus '0' .. '9']
let octal_ascii = [%sedlex.regexp? "0o", Plus '0' .. '7']

let hex_ascii =
  [%sedlex.regexp? "0x", Plus ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F')]

let rec nom buf = match%sedlex buf with Plus any_blank -> nom buf | _ -> ()

let string buf =
  let buffer = Buffer.create 10 in
  let rec read_string buf =
    match%sedlex buf with
      | {|\"|} ->
          Buffer.add_char buffer '"';
          read_string buf
      | '"' -> STRING (Buffer.contents buffer)
      | Star (Compl '"') ->
          Buffer.add_string buffer (Utf8.lexeme buf);
          read_string buf
      | _ -> assert false
  in
  read_string buf

let digit_value c =
  let open Stdlib in
  match c with
    | 'a' .. 'f' -> 10 + Char.code c - Char.code 'a'
    | 'A' .. 'F' -> 10 + Char.code c - Char.code 'A'
    | '0' .. '9' -> Char.code c - Char.code '0'
    | _ -> assert false

let num_value buffer ~base ~first =
  let buf = Utf8.lexeme buffer in
  let c = ref 0 in
  for i = first to String.length buf - 1 do
    let v = digit_value buf.[i] in
    assert (v < base);
    c := (base * !c) + v
  done;
  ignore !c;
  buf

let token buf =
  nom buf;
  match%sedlex buf with
    | eof -> EOF
    | "" -> EOF
    | '-' -> MINUS
    | '+' -> PLUS
    | '"' -> string buf
    | ':' -> COLON
    | '[' -> LSQUARE
    | ']' -> RSQUARE
    | '{' -> LBRACKET
    | '}' -> RBRACKET
    | ',' -> COMMA
    | "true" -> BOOL true
    | "false" -> BOOL false
    | hex_ascii ->
        let number = num_value ~base:16 ~first:2 buf in
        NUMBER number
    | octal_ascii ->
        let number = num_value ~base:8 ~first:2 buf in
        NUMBER number
    | decimal_ascii ->
        let number = num_value ~base:10 ~first:0 buf in
        NUMBER number
    | _ ->
        let position = fst @@ lexing_positions buf in
        let tok = Utf8.lexeme buf in
        raise
        @@ LexError (position, Printf.sprintf "unexpected character %S" tok)

let rec read_all buf =
  match token buf with
    | EOF -> Printf.eprintf "DONE"
    | tok ->
        ignore tok;
        read_all buf

let () =
  match Array.to_list Sys.argv with
    | [_; "chan"; file] ->
        let ic = open_in_bin file in
        let buf = Sedlexing.Utf8.from_channel ic in
        read_all buf;
        close_in ic
    | [_; "gen"; file] ->
        let ic = open_in_bin file in
        let gen () = In_channel.input_char ic in
        let buf = Sedlexing.Utf8.from_gen gen in
        read_all buf;
        close_in ic
    | [_; "string"; file] ->
        let ic = open_in_bin file in
        let s = In_channel.input_all ic in
        let buf = Sedlexing.Utf8.from_string s in
        read_all buf;
        close_in ic
    | [_; mode; file] ->
        failwith (Printf.sprintf "invalid_argument %s %s" mode file)
    | l -> failwith (Printf.sprintf "wrong number of args %d" (List.length l))
