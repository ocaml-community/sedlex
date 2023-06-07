(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

exception InvalidCodepoint of int
exception MalFormed

module Uchar = struct
  include Uchar

  let of_int x =
    if Uchar.is_valid x then Uchar.unsafe_of_int x else raise MalFormed
end

(* Absolute position from the beginning of the stream *)
type apos = int

type lexbuf = {
  refill : Uchar.t array -> int -> int -> int;
  mutable buf : Uchar.t array;
  mutable len : int;
  (* Number of meaningful char in buffer *)
  mutable offset : apos;
  (* Position of the first char in buffer
      in the input stream *)
  mutable pos : int;
  (* pos is the index in the buffer *)
  mutable curr_bol : int;
  (* bol is the index in the input stream but not buffer *)
  mutable curr_line : int;
  (* start from 1, if it is 0, we would not track postion info for you *)
  mutable start_pos : int;
  (* First char we need to keep visible *)
  mutable start_bol : int;
  mutable start_line : int;
  mutable marked_pos : int;
  mutable marked_bol : int;
  mutable marked_line : int;
  mutable marked_val : int;
  mutable filename : string;
  mutable finished : bool;
}

let chunk_size = 512

let empty_lexbuf =
  {
    refill = (fun _ _ _ -> assert false);
    buf = [||];
    len = 0;
    offset = 0;
    pos = 0;
    curr_bol = 0;
    curr_line = 0;
    start_pos = 0;
    start_bol = 0;
    start_line = 0;
    marked_pos = 0;
    marked_bol = 0;
    marked_line = 0;
    marked_val = 0;
    filename = "";
    finished = false;
  }

let create f =
  {
    empty_lexbuf with
    refill = f;
    buf = Array.make chunk_size (Uchar.of_int 0);
    curr_line = 1;
  }

let set_position lexbuf position =
  lexbuf.offset <- position.Lexing.pos_cnum - lexbuf.pos;
  lexbuf.curr_bol <- position.Lexing.pos_bol;
  lexbuf.curr_line <- position.Lexing.pos_lnum

let set_filename lexbuf fname = lexbuf.filename <- fname

let refill_buf_from_gen gen =
  let malformed = ref false in
  fun buf pos len ->
    if !malformed then raise MalFormed;
    let rec aux i =
      if i >= len then len
      else (
        match gen () with
          | Some c ->
              buf.(pos + i) <- c;
              aux (i + 1)
          | None -> i
          | exception MalFormed when i <> 0 ->
              malformed := true;
              i)
    in
    aux 0

let from_gen gen = create (refill_buf_from_gen gen)

let from_int_array a =
  let i = ref 0 in
  (* copy the array to not be affected by future mutations *)
  let a = Array.copy a in
  let len = Array.length a in
  let gen () =
    if !i = len then None
    else (
      let u = Uchar.of_int (Array.get a !i) in
      incr i;
      Some u)
  in
  from_gen gen

let from_uchar_array buf =
  (* copy the array to not be affected by future mutations *)
  let buf = Array.copy buf in
  let len = Array.length buf in
  { empty_lexbuf with buf; len; finished = true }

let refill lexbuf =
  if lexbuf.len + chunk_size > Array.length lexbuf.buf then begin
    let s = lexbuf.start_pos in
    let ls = lexbuf.len - s in
    if ls + chunk_size <= Array.length lexbuf.buf then
      Array.blit lexbuf.buf s lexbuf.buf 0 ls
    else begin
      let newlen = (Array.length lexbuf.buf + chunk_size) * 2 in
      let newbuf = Array.make newlen (Uchar.of_int 0) in
      Array.blit lexbuf.buf s newbuf 0 ls;
      lexbuf.buf <- newbuf
    end;
    lexbuf.len <- ls;
    lexbuf.offset <- lexbuf.offset + s;
    lexbuf.pos <- lexbuf.pos - s;
    lexbuf.marked_pos <- lexbuf.marked_pos - s;
    lexbuf.start_pos <- 0
  end;
  let n = lexbuf.refill lexbuf.buf lexbuf.pos chunk_size in
  if n = 0 then lexbuf.finished <- true else lexbuf.len <- lexbuf.len + n

let new_line lexbuf =
  if lexbuf.curr_line != 0 then lexbuf.curr_line <- lexbuf.curr_line + 1;
  lexbuf.curr_bol <- lexbuf.pos + lexbuf.offset

let next lexbuf =
  if (not lexbuf.finished) && lexbuf.pos = lexbuf.len then refill lexbuf;
  if lexbuf.finished && lexbuf.pos = lexbuf.len then None
  else begin
    let ret = lexbuf.buf.(lexbuf.pos) in
    lexbuf.pos <- lexbuf.pos + 1;
    if ret = Uchar.of_int 10 then new_line lexbuf;
    Some ret
  end

let __private__next_int lexbuf : int =
  if (not lexbuf.finished) && lexbuf.pos = lexbuf.len then refill lexbuf;
  if lexbuf.finished && lexbuf.pos = lexbuf.len then -1
  else begin
    let ret = lexbuf.buf.(lexbuf.pos) in
    lexbuf.pos <- lexbuf.pos + 1;
    if ret = Uchar.of_int 10 then new_line lexbuf;
    Uchar.to_int ret
  end

let mark lexbuf i =
  lexbuf.marked_pos <- lexbuf.pos;
  lexbuf.marked_bol <- lexbuf.curr_bol;
  lexbuf.marked_line <- lexbuf.curr_line;
  lexbuf.marked_val <- i

let start lexbuf =
  lexbuf.start_pos <- lexbuf.pos;
  lexbuf.start_bol <- lexbuf.curr_bol;
  lexbuf.start_line <- lexbuf.curr_line;
  mark lexbuf (-1)

let backtrack lexbuf =
  lexbuf.pos <- lexbuf.marked_pos;
  lexbuf.curr_bol <- lexbuf.marked_bol;
  lexbuf.curr_line <- lexbuf.marked_line;
  lexbuf.marked_val

let rollback lexbuf =
  lexbuf.pos <- lexbuf.start_pos;
  lexbuf.curr_bol <- lexbuf.start_bol;
  lexbuf.curr_line <- lexbuf.start_line

let lexeme_start lexbuf = lexbuf.start_pos + lexbuf.offset
let lexeme_end lexbuf = lexbuf.pos + lexbuf.offset
let loc lexbuf = (lexbuf.start_pos + lexbuf.offset, lexbuf.pos + lexbuf.offset)
let lexeme_length lexbuf = lexbuf.pos - lexbuf.start_pos

let sub_lexeme lexbuf pos len =
  Array.sub lexbuf.buf (lexbuf.start_pos + pos) len

let lexeme lexbuf =
  Array.sub lexbuf.buf lexbuf.start_pos (lexbuf.pos - lexbuf.start_pos)

let lexeme_char lexbuf pos = lexbuf.buf.(lexbuf.start_pos + pos)

let lexing_positions lexbuf =
  let start_p =
    {
      Lexing.pos_fname = lexbuf.filename;
      pos_lnum = lexbuf.start_line;
      pos_cnum = lexbuf.start_pos + lexbuf.offset;
      pos_bol = lexbuf.start_bol;
    }
  and curr_p =
    {
      Lexing.pos_fname = lexbuf.filename;
      pos_lnum = lexbuf.curr_line;
      pos_cnum = lexbuf.pos + lexbuf.offset;
      pos_bol = lexbuf.curr_bol;
    }
  in
  (start_p, curr_p)

let with_tokenizer lexer' lexbuf =
  let lexer () =
    let token = lexer' lexbuf in
    let start_p, curr_p = lexing_positions lexbuf in
    (token, start_p, curr_p)
  in
  lexer

module type Buf = sig
  type t

  val available : t -> int
  val ensure_bytes_available : t -> can_refill:bool -> int -> unit
  val get : t -> int -> char
  val advance : t -> int -> unit
  val raw_buf : t -> Bytes.t
  val raw_pos : t -> int
end

exception Missing_input

module T_channel = struct
  type t = {
    b : Bytes.t;
    ic : in_channel;
    mutable len : int;
    mutable pos : int;
  }

  let min_buffer_size = 64

  let create ic len : t =
    let len = max len min_buffer_size in
    { b = Bytes.create len; ic; len = 0; pos = 0 }

  let available (t : t) = t.len - t.pos

  let rec ensure_bytes_available (t : t) ~can_refill n =
    if available t >= n then ()
    else if can_refill then (
      let len = t.len - t.pos in
      if t.pos > 0 then (
        Bytes.blit t.b t.pos t.b 0 len;
        t.pos <- 0);
      let read = input t.ic t.b len (Bytes.length t.b - len) in
      t.len <- len + read;
      if read = 0 then raise Missing_input
      else ensure_bytes_available t ~can_refill n)
    else raise Missing_input

  let ensure_bytes_available t ~can_refill n =
    (* [n] should not exceed the size of the buffer. Here we are
       conservative and make sure it doesn't exceed the mininum size
       for the buffer. *)
    if n <= 0 || n > min_buffer_size then invalid_arg "Sedlexing.Chan.ensure";
    ensure_bytes_available t ~can_refill n

  let get (t : t) i = Bytes.get t.b (t.pos + i)

  let advance (t : t) n =
    if t.pos + n > t.len then invalid_arg "advance";
    t.pos <- t.pos + n

  let raw_buf (t : t) = t.b
  let raw_pos (t : t) = t.pos
end

module T_gen = struct
  type t = {
    b : Bytes.t;
    gen : unit -> char option;
    mutable len : int;
    mutable pos : int;
  }

  let min_buffer_size = 64
  let create gen len : t = { b = Bytes.create len; gen; len = 0; pos = 0 }
  let available (t : t) = t.len - t.pos

  let rec ensure_bytes_available (t : t) n =
    let available = available t in
    if available >= n then ()
    else (
      match t.gen () with
        | None -> raise Missing_input
        | Some c ->
            if t.pos > 0 then
              if available = 0 then (
                t.pos <- 0;
                t.len <- 0)
              else (
                Bytes.blit t.b t.pos t.b 0 available;
                t.pos <- 0;
                t.len <- available);
            Bytes.set t.b t.len c;
            t.len <- t.len + 1;
            ensure_bytes_available t n)

  let ensure_bytes_available t ~can_refill:_ n =
    (* [n] should not exceed the size of the buffer. Here we are
       conservative and make sure it doesn't exceed the mininum size
       for the buffer. *)
    if n <= 0 || n > min_buffer_size then invalid_arg "Sedlexing.Chan.ensure";
    ensure_bytes_available t n

  let get (t : t) i = Bytes.get t.b (t.pos + i)

  let advance (t : t) n =
    if t.pos + n > t.len then invalid_arg "advance";
    t.pos <- t.pos + n;
    assert (t.pos <= t.len)

  let raw_buf (t : t) = t.b
  let raw_pos (t : t) = t.pos
end

module T_bytes = struct
  type t = { b : Bytes.t; max : int; mutable pos : int }

  let create b pos len : t =
    let max = min (Bytes.length b) (pos + len) in
    { b; max; pos }

  let available (t : t) = t.max - t.pos

  let ensure_bytes_available (t : t) n =
    let available = available t in
    if available >= n then () else raise Missing_input

  let ensure_bytes_available t ~can_refill:_ n =
    (* [n] should not exceed the size of the buffer. Here we are
       conservative and make sure it doesn't exceed the mininum size
       for the buffer. *)
    if n <= 0 then invalid_arg "Sedlexing.Chan.ensure";
    ensure_bytes_available t n

  let get (t : t) i = Bytes.get t.b (t.pos + i)

  let advance (t : t) n =
    if t.pos + n > t.max then invalid_arg "advance";
    t.pos <- t.pos + n

  let raw_buf (t : t) = t.b
  let raw_pos (t : t) = t.pos
end

let make_from_buf (type t) (module B : Buf with type t = t) (t : t)
    ~max_bytes_per_uchar:_ ~min_bytes_per_uchar
    ~(read_uchar :
       (module Buf with type t = t) -> can_refill:bool -> t -> Uchar.t) =
  let malformed = ref false in
  let refill buf pos len =
    if !malformed then raise MalFormed;
    let rec loop i =
      if i = len then i
      else (
        match
          (* we refill our bytes buffer only if we haven't refilled any uchar yet. *)
          let can_refill = i = 0 in
          B.ensure_bytes_available t ~can_refill min_bytes_per_uchar;
          read_uchar (module B) ~can_refill t
        with
          | c ->
              buf.(pos + i) <- c;
              loop (i + 1)
          | exception MalFormed when i <> 0 ->
              malformed := true;
              i
          | exception Missing_input ->
              if i = 0 && B.available t > 0 then raise MalFormed;
              i)
    in
    loop 0
  in
  create refill

let make_from_channel ic ~max_bytes_per_uchar ~min_bytes_per_uchar ~read_uchar =
  let t = T_channel.create ic (chunk_size * max_bytes_per_uchar) in
  make_from_buf
    (module T_channel)
    t ~max_bytes_per_uchar ~min_bytes_per_uchar ~read_uchar

let make_from_char_gen ic ~max_bytes_per_uchar ~min_bytes_per_uchar ~read_uchar
    =
  let t = T_gen.create ic max_bytes_per_uchar in
  make_from_buf
    (module T_gen)
    t ~max_bytes_per_uchar ~min_bytes_per_uchar ~read_uchar

let make_from_substring (s : string) pos len ~max_bytes_per_uchar
    ~min_bytes_per_uchar ~read_uchar =
  let b = Bytes.unsafe_of_string s in
  let t = T_bytes.create b pos len in
  make_from_buf
    (module T_bytes)
    t ~max_bytes_per_uchar ~min_bytes_per_uchar ~read_uchar

module Latin1 = struct
  let read_uchar (type t) (module Buf : Buf with type t = t) ~can_refill:_
      (t : t) =
    let c = Uchar.of_char (Buf.get t 0) in
    Buf.advance t 1;
    c

  let min_bytes_per_uchar = 1
  let max_bytes_per_uchar = 1

  let from_channel ic =
    make_from_channel ic ~min_bytes_per_uchar ~max_bytes_per_uchar ~read_uchar

  let from_gen gen =
    make_from_char_gen gen ~min_bytes_per_uchar ~max_bytes_per_uchar ~read_uchar

  let from_string s =
    make_from_substring s 0 (String.length s) ~min_bytes_per_uchar
      ~max_bytes_per_uchar ~read_uchar

  let to_latin1 c =
    if Uchar.is_char c then Uchar.to_char c
    else raise (InvalidCodepoint (Uchar.to_int c))

  let lexeme_char lexbuf pos = to_latin1 (lexeme_char lexbuf pos)

  let sub_lexeme lexbuf pos len =
    let s = Bytes.create len in
    for i = 0 to len - 1 do
      Bytes.set s i (to_latin1 lexbuf.buf.(lexbuf.start_pos + pos + i))
    done;
    Bytes.to_string s

  let lexeme lexbuf = sub_lexeme lexbuf 0 (lexbuf.pos - lexbuf.start_pos)
end

module Utf8 = struct
  module Helper = struct
    (* http://www.faqs.org/rfcs/rfc3629.html *)

    let width = function
      | '\000' .. '\127' -> 1
      | '\192' .. '\223' -> 2
      | '\224' .. '\239' -> 3
      | '\240' .. '\247' -> 4
      | _ -> raise MalFormed

    let next s i =
      let c1 = s.[i] in
      match width c1 with
        | 1 -> Char.code c1
        | 2 ->
            let n1 = Char.code c1 in
            let n2 = Char.code s.[i + 1] in
            if n2 lsr 6 != 0b10 then raise MalFormed;
            ((n1 land 0x1f) lsl 6) lor (n2 land 0x3f)
        | 3 ->
            let n1 = Char.code c1 in
            let n2 = Char.code s.[i + 1] in
            let n3 = Char.code s.[i + 2] in
            if n2 lsr 6 != 0b10 || n3 lsr 6 != 0b10 then raise MalFormed;
            let p =
              ((n1 land 0x0f) lsl 12)
              lor ((n2 land 0x3f) lsl 6)
              lor (n3 land 0x3f)
            in
            if p >= 0xd800 && p <= 0xdf00 then raise MalFormed;
            p
        | 4 ->
            let n1 = Char.code c1 in
            let n2 = Char.code s.[i + 1] in
            let n3 = Char.code s.[i + 2] in
            let n4 = Char.code s.[i + 3] in
            if n2 lsr 6 != 0b10 || n3 lsr 6 != 0b10 || n4 lsr 6 != 0b10 then
              raise MalFormed;
            ((n1 land 0x07) lsl 18)
            lor ((n2 land 0x3f) lsl 12)
            lor ((n3 land 0x3f) lsl 6)
            lor (n4 land 0x3f)
        | _ -> assert false

    (**************************)

    let store b p =
      if p <= 0x7f then Buffer.add_char b (Char.chr p)
      else if p <= 0x7ff then (
        Buffer.add_char b (Char.chr (0xc0 lor (p lsr 6)));
        Buffer.add_char b (Char.chr (0x80 lor (p land 0x3f))))
      else if p <= 0xffff then (
        if p >= 0xd800 && p < 0xe000 then raise MalFormed;
        Buffer.add_char b (Char.chr (0xe0 lor (p lsr 12)));
        Buffer.add_char b (Char.chr (0x80 lor ((p lsr 6) land 0x3f)));
        Buffer.add_char b (Char.chr (0x80 lor (p land 0x3f))))
      else if p <= 0x10ffff then (
        Buffer.add_char b (Char.chr (0xf0 lor (p lsr 18)));
        Buffer.add_char b (Char.chr (0x80 lor ((p lsr 12) land 0x3f)));
        Buffer.add_char b (Char.chr (0x80 lor ((p lsr 6) land 0x3f)));
        Buffer.add_char b (Char.chr (0x80 lor (p land 0x3f))))
      else raise MalFormed

    let string_of_uchar_array a apos len =
      let b = Buffer.create (len * 4) in
      let rec aux apos len =
        if len > 0 then (
          store b (Uchar.to_int a.(apos));
          aux (succ apos) (pred len))
        else Buffer.contents b
      in
      aux apos len
  end

  let read_uchar (type t) (module Buf : Buf with type t = t) ~can_refill (t : t)
      =
    let w = Helper.width (Buf.get t 0) in
    Buf.ensure_bytes_available t ~can_refill w;
    let c =
      Helper.next (Bytes.unsafe_to_string (Buf.raw_buf t)) (Buf.raw_pos t)
    in
    Buf.advance t w;
    Uchar.of_int c

  let min_bytes_per_uchar = 1
  let max_bytes_per_uchar = 4

  let from_channel ic =
    make_from_channel ic ~min_bytes_per_uchar ~max_bytes_per_uchar ~read_uchar

  let from_gen gen =
    make_from_char_gen gen ~min_bytes_per_uchar ~max_bytes_per_uchar ~read_uchar

  let from_string s =
    make_from_substring s 0 (String.length s) ~min_bytes_per_uchar
      ~max_bytes_per_uchar ~read_uchar

  let sub_lexeme lexbuf pos len =
    Helper.string_of_uchar_array lexbuf.buf (lexbuf.start_pos + pos) len

  let lexeme lexbuf = sub_lexeme lexbuf 0 (lexbuf.pos - lexbuf.start_pos)
end

module Utf16 = struct
  type byte_order = Little_endian | Big_endian

  module Helper = struct
    (* http://www.ietf.org/rfc/rfc2781.txt *)

    let number_of_pair bo c1 c2 =
      match bo with
        | Little_endian -> (c2 lsl 8) + c1
        | Big_endian -> (c1 lsl 8) + c2

    let char_pair_of_number bo num =
      match bo with
        | Little_endian ->
            (Char.chr (num land 0xFF), Char.chr ((num lsr 8) land 0xFF))
        | Big_endian ->
            (Char.chr ((num lsr 8) land 0xFF), Char.chr (num land 0xFF))

    let get_bo bo c1 c2 =
      match !bo with
        | Some o -> o
        | None ->
            let o =
              match (c1, c2) with
                | 0xff, 0xfe -> Little_endian
                | _ -> Big_endian
            in
            bo := Some o;
            o

    let store bo buf code =
      if code < 0x10000 then (
        let c1, c2 = char_pair_of_number bo code in
        Buffer.add_char buf c1;
        Buffer.add_char buf c2)
      else (
        let u' = code - 0x10000 in
        let w1 = 0xd800 + (u' lsr 10) and w2 = 0xdc00 + (u' land 0x3ff) in
        let c1, c2 = char_pair_of_number bo w1
        and c3, c4 = char_pair_of_number bo w2 in
        Buffer.add_char buf c1;
        Buffer.add_char buf c2;
        Buffer.add_char buf c3;
        Buffer.add_char buf c4)

    let string_of_uchar_array bo a apos len bom =
      let b = Buffer.create ((len * 4) + 2) in
      (* +2 for the BOM *)
      if bom then store bo b 0xfeff;
      (* first, store the BOM *)
      let rec aux apos len =
        if len > 0 then (
          store bo b (Uchar.to_int a.(apos));
          aux (succ apos) (pred len))
        else Buffer.contents b
      in
      aux apos len
  end

  let min_bytes_per_uchar = 2
  let max_bytes_per_uchar = 4

  let read_uchar bo (type t) (module Buf : Buf with type t = t) ~can_refill
      (t : t) =
    let n1 = Char.code (Buf.get t 0) in
    let n2 = Char.code (Buf.get t 1) in
    let o = Helper.get_bo bo n1 n2 in
    let w1 = Helper.number_of_pair o n1 n2 in
    if w1 = 0xfffe then raise (InvalidCodepoint w1);
    if w1 < 0xd800 || 0xdfff < w1 then (
      Buf.advance t 2;
      Uchar.of_int w1)
    else if w1 <= 0xdbff then (
      Buf.ensure_bytes_available t ~can_refill max_bytes_per_uchar;
      let n3 = Char.code (Buf.get t 2) in
      let n4 = Char.code (Buf.get t 3) in
      let w2 = Helper.number_of_pair o n3 n4 in
      if w2 < 0xdc00 || w2 > 0xdfff then raise MalFormed;
      let upper10 = (w1 land 0x3ff) lsl 10 and lower10 = w2 land 0x3ff in
      Buf.advance t 4;
      Uchar.of_int (0x10000 + upper10 + lower10))
    else raise MalFormed

  let from_channel ic opt_bo =
    let read_uchar = read_uchar (ref opt_bo) in
    make_from_channel ic ~min_bytes_per_uchar ~max_bytes_per_uchar ~read_uchar

  let from_gen gen opt_bo =
    let read_uchar = read_uchar (ref opt_bo) in
    make_from_char_gen gen ~min_bytes_per_uchar ~max_bytes_per_uchar ~read_uchar

  let from_string s opt_bo =
    let read_uchar = read_uchar (ref opt_bo) in
    make_from_substring s 0 (String.length s) ~min_bytes_per_uchar
      ~max_bytes_per_uchar ~read_uchar

  let sub_lexeme lb pos len bo bom =
    Helper.string_of_uchar_array bo lb.buf (lb.start_pos + pos) len bom

  let lexeme lb bo bom = sub_lexeme lb 0 (lb.pos - lb.start_pos) bo bom
end
