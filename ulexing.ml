(* Unicode code point *)
type uchar = int
let eof = -1

(* Absolute position from the beginning of the stream *)
type apos = int

type lexbuf = {
  refill: (uchar array -> int -> int -> int);
  mutable buf: uchar array;
  mutable len: int;    (* Number of meaningful uchar in buffer *)
  mutable offset: apos; (* Position of the first uchar in buffer
			    in the input stream *)
  mutable pos : int;
  mutable start : int; (* First uchar we need to keep visible *)

  mutable marked_pos : int;
  mutable marked_val : int;
}


let chunk_size = 512

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
  if lexbuf.pos = lexbuf.len then refill lexbuf;
  lexbuf.buf.(lexbuf.pos)

let mark lexbuf i =
  lexbuf.marked_pos <- lexbuf.pos;
  lexbuf.marked_val <- i

let backtrack lexbuf =
  lexbuf.pos <- lexbuf.marked_pos;
  lexbuf.marked_val

let pos lexbuf : apos =
  lexbuf.pos + lexbuf.offset

let rel_pos lexbuf (p : apos) : int =
  let p = p - lexbuf.offset in
  if (p < 0) || (p >= lexbuf.len) then failwith "Ulexing.backtrack";
  p

let subword lexbuf (i : apos) (j : apos) =
  let i = rel_pos lexbuf i and j = rel_pos lexbuf j in
  Array.sub lexbuf.buf i (j - i)

