exception Error

let eof = -1

(* Absolute position from the beginning of the stream *)
type apos = int

type lexbuf = {
  refill: (int array -> int -> int -> int);
  mutable buf: int array;
  mutable len: int;    (* Number of meaningful char in buffer *)
  mutable offset: apos; (* Position of the first char in buffer
			    in the input stream *)
  mutable pos : int;
  mutable start : int; (* First char we need to keep visible *)

  mutable marked_pos : int;
  mutable marked_val : int;

  mutable finished: bool;
}

let chunk_size = 512

let empty_lexbuf = {
  refill = (fun _ _ _ -> assert false);
  buf = [| |];
  len = 0;
  offset = 0;
  pos = 0;
  start = 0;
  marked_pos = 0;
  marked_val = 0;
  finished = false;
}

let create f = {
  empty_lexbuf with
    refill = f;
    buf = Array.create chunk_size 0;
}

let from_latin1_string s = 
  let len = String.length s in
  {
    empty_lexbuf with
      buf = Array.init len (fun i -> Char.code s.[i]);
      len = len;
      finished = true;
  }

let from_int_array a =
  let len = Array.length a in
  {
    empty_lexbuf with
      buf = Array.init len (fun i -> a.(i));
      len = len;
      finished = true;
  }


let from_utf8_string s =
  from_int_array (Utf8.to_int_array s 0 (String.length s))

let refill lexbuf =
  if lexbuf.len + chunk_size > Array.length lexbuf.buf 
  then begin
    let s = lexbuf.start in
    let ls = lexbuf.len - s in
    if ls + chunk_size <= Array.length lexbuf.buf then
      Array.blit lexbuf.buf s lexbuf.buf 0 ls
    else begin
      let newlen = (Array.length lexbuf.buf + chunk_size) * 2 in
      let newbuf = Array.create newlen 0 in
      Array.blit lexbuf.buf s newbuf 0 ls;
      lexbuf.buf <- newbuf
    end;
    lexbuf.len <- ls;
    lexbuf.offset <- lexbuf.offset + s;
    lexbuf.pos <- lexbuf.pos - s;
    lexbuf.marked_pos <- lexbuf.marked_pos - s;
    lexbuf.start <- 0
  end;
  let n = lexbuf.refill lexbuf.buf lexbuf.pos chunk_size in
  if (n = 0) 
  then begin
    lexbuf.buf.(lexbuf.len) <- eof;
    lexbuf.len <- lexbuf.len + 1;
  end
  else lexbuf.len <- lexbuf.len + n

let next lexbuf =
  let i = 
    if lexbuf.pos = lexbuf.len then 
      if lexbuf.finished then eof
      else (refill lexbuf; lexbuf.buf.(lexbuf.pos))
    else lexbuf.buf.(lexbuf.pos)
  in
  if i = eof then lexbuf.finished <- true else lexbuf.pos <- lexbuf.pos + 1;
  i

let start lexbuf =
  lexbuf.start <- lexbuf.pos;
  lexbuf.marked_pos <- lexbuf.pos;
  lexbuf.marked_val <- (-1)

let mark lexbuf i =
  lexbuf.marked_pos <- lexbuf.pos;
  lexbuf.marked_val <- i

let backtrack lexbuf =
  lexbuf.pos <- lexbuf.marked_pos;
  lexbuf.marked_val

let lexeme_start lexbuf = lexbuf.start + lexbuf.offset
let lexeme_end lexbuf = lexbuf.pos + lexbuf.offset

let lexeme_sub lexbuf pos len = Array.sub lexbuf.buf (lexbuf.start + pos) len
let lexeme lexbuf = Array.sub lexbuf.buf (lexbuf.start) (lexbuf.pos - lexbuf.start)
let lexeme_char lexbuf pos = lexbuf.buf.(lexbuf.start + pos)

let to_latin1 c =
  if (c >= 0) && (c < 256) 
  then Char.chr c 
  else failwith (Printf.sprintf "to_latin1: code point %i cannot be encoded in Latin1" c)

let latin1_lexeme_char lexbuf pos = to_latin1 (lexeme_char lexbuf pos)
let latin1_lexeme_sub lexbuf pos len =
  let s = String.create len in
  for i = 0 to len - 1 do s.[i] <- to_latin1 lexbuf.buf.(lexbuf.start + pos + i) done;
  s
let latin1_lexeme lexbuf = latin1_lexeme_sub lexbuf 0 (lexbuf.pos - lexbuf.start)

let utf8_lexeme_sub lexbuf pos len =
  Utf8.from_int_array lexbuf.buf (lexbuf.start + pos) len

let utf8_lexeme lexbuf = utf8_lexeme_sub lexbuf 0 (lexbuf.pos - lexbuf.start)
