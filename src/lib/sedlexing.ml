(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

exception InvalidCodepoint of int
exception MalFormed

module Uchar = struct
  include Uchar

  let of_int x =
    if Uchar.is_valid x then Uchar.unsafe_of_int x else raise MalFormed

  (* As in the standard library since OCaml 4.14, redefined here so that
     they are inlined. *)

  let[@inline] utf_8_byte_length u =
    let c = to_int u in
    if c < 0x80 then 1
    else if c < 0x800 then 2
    else if c < 0x10000 then 3
    else 4

  let[@inline] utf_16_byte_length u = if to_int u < 0x10000 then 2 else 4
end

module Bytes = struct
  include Bytes

  (* [set_utf_8_uchar], [set_utf_16be_uchar] and [set_utf_16le_uchar] as in
     the standard library since OCaml 4.14, redefined here so that they are
     inlined: they write the encoding of a code point at an index and return
     its length, or 0 without writing anything when it does not fit. *)

  let[@inline] unsafe_set_uint8 b i x = unsafe_set b i (Char.unsafe_chr x)

  let[@inline] set_utf_8_uchar b i u =
    let c = Uchar.to_int u in
    let room = length b - i in
    if i < 0 || room <= 0 then raise (Invalid_argument "index out of bounds")
    else if c < 0x80 then begin
      unsafe_set_uint8 b i c;
      1
    end
    else if c < 0x800 then
      if room < 2 then 0
      else begin
        unsafe_set_uint8 b i (0xC0 lor (c lsr 6));
        unsafe_set_uint8 b (i + 1) (0x80 lor (c land 0x3F));
        2
      end
    else if c < 0x10000 then
      if room < 3 then 0
      else begin
        unsafe_set_uint8 b i (0xE0 lor (c lsr 12));
        unsafe_set_uint8 b (i + 1) (0x80 lor ((c lsr 6) land 0x3F));
        unsafe_set_uint8 b (i + 2) (0x80 lor (c land 0x3F));
        3
      end
    else if room < 4 then 0
    else begin
      unsafe_set_uint8 b i (0xF0 lor (c lsr 18));
      unsafe_set_uint8 b (i + 1) (0x80 lor ((c lsr 12) land 0x3F));
      unsafe_set_uint8 b (i + 2) (0x80 lor ((c lsr 6) land 0x3F));
      unsafe_set_uint8 b (i + 3) (0x80 lor (c land 0x3F));
      4
    end

  (* [hi] and [lo] are the offsets of the high and low bytes of a 16-bit
     unit, which give the byte order. *)
  let[@inline] unsafe_set_uint16 ~hi ~lo b i x =
    unsafe_set_uint8 b (i + hi) (x lsr 8);
    unsafe_set_uint8 b (i + lo) (x land 0xFF)

  let[@inline] set_utf_16_uchar ~hi ~lo b i u =
    let c = Uchar.to_int u in
    let room = length b - i in
    if i < 0 || room <= 0 then raise (Invalid_argument "index out of bounds")
    else if c < 0x10000 then
      if room < 2 then 0
      else begin
        unsafe_set_uint16 ~hi ~lo b i c;
        2
      end
    else if room < 4 then 0
    else begin
      (* a surrogate pair *)
      let c = c - 0x10000 in
      unsafe_set_uint16 ~hi ~lo b i (0xD800 lor (c lsr 10));
      unsafe_set_uint16 ~hi ~lo b (i + 2) (0xDC00 lor (c land 0x3FF));
      4
    end

  let[@inline] set_utf_16be_uchar b i u = set_utf_16_uchar ~hi:0 ~lo:1 b i u
  let[@inline] set_utf_16le_uchar b i u = set_utf_16_uchar ~hi:1 ~lo:0 b i u
end

(* shadow polymorphic equal *)
let ( = ) (a : int) b = a = b
let ( >>| ) o f = match o with Some x -> Some (f x) | None -> None

(* Absolute position from the beginning of the stream *)
type apos = int

type lexbuf = {
  refill : Uchar.t array -> int -> int -> int;
  bytes_per_char : Uchar.t -> int;
  mutable buf : Uchar.t array;
  (* Number of valid uchars in [buf] (from index 0 to len-1). *)
  mutable len : int;
  (* Cumulative uchar count: number of uchars discarded before buf[0].
     Absolute uchar position of buf[i] = offset + i. *)
  mutable offset : apos;
  (* Cumulative byte count: number of bytes discarded before buf[0]. *)
  mutable bytes_offset : apos;
  (* Current read position in [buf] (buffer-relative index, 0-based). *)
  mutable pos : int;
  (* Current read position in bytes (buffer-relative). *)
  mutable bytes_pos : int;
  (* Absolute position of the beginning of the current line, in uchar. *)
  mutable curr_bol : int;
  (* Absolute position of the beginning of the current line, in bytes. *)
  mutable curr_bytes_bol : int;
  (* Index of the current line in the input stream. *)
  mutable curr_line : int;
  (* Token start position in [buf], in uchars (buffer-relative). *)
  mutable start_pos : int;
  (* Token start position in bytes (buffer-relative). *)
  mutable start_bytes_pos : int;
  (* Absolute beginning-of-line position at token start, in uchars. *)
  mutable start_bol : int;
  (* Absolute beginning-of-line position at token start, in bytes. *)
  mutable start_bytes_bol : int;
  (* Line number at token start (starts from 1). *)
  mutable start_line : int;
  (* Backtrack snapshot: saved by [mark], restored by [backtrack]. *)
  mutable marked_pos : int;
  mutable marked_bytes_pos : int;
  mutable marked_bol : int;
  mutable marked_bytes_bol : int;
  mutable marked_line : int;
  (* The rule index stored by [mark]. *)
  mutable marked_val : int;
  mutable filename : string;
  (* True when the input source is exhausted. *)
  mutable finished : bool;
  (* Memory cells for tagged DFA transitions (as-bindings).
     A single int array stores both positions and discriminator values,
     distinguished by range:
     - positions: buffer-relative uchar indices (>= 0), adjusted by
       [refill] when the buffer is compacted, and converted to
       token-relative offsets on read by [__private__mem_pos].
     - discriminator values: stored as [-(v + 2)], always <= -2,
       disjoint from positions.
     The cells are not cleared between tokens: an action only reads cells
     written on the path of its match.
     [mark] and [backtrack] leave the cells alone: those read after a match
     are only written on entering an accepting state, just before [mark]. *)
  mutable __private__mem : int array;
}

let chunk_size = 512

let empty_lexbuf bytes_per_char =
  {
    refill = (fun _ _ _ -> assert false);
    bytes_per_char;
    buf = [||];
    len = 0;
    offset = 0;
    bytes_offset = 0;
    pos = 0;
    bytes_pos = 0;
    curr_bol = 0;
    curr_bytes_bol = 0;
    curr_line = 1;
    start_pos = 0;
    start_bytes_pos = 0;
    start_bol = 0;
    start_bytes_bol = 0;
    start_line = 0;
    marked_pos = 0;
    marked_bytes_pos = 0;
    marked_bol = 0;
    marked_bytes_bol = 0;
    marked_line = 0;
    marked_val = 0;
    filename = "";
    finished = false;
    __private__mem = [||];
  }

let dummy_uchar = Uchar.of_int 0
let nl_uchar = Uchar.of_int 10

let create ?(bytes_per_char = fun _ -> 1) refill =
  {
    (empty_lexbuf bytes_per_char) with
    refill;
    buf = Array.make chunk_size dummy_uchar;
  }

let set_position ?bytes_position lexbuf position =
  lexbuf.offset <- position.Lexing.pos_cnum - lexbuf.pos;
  lexbuf.curr_bol <- position.Lexing.pos_bol;
  lexbuf.curr_line <- position.Lexing.pos_lnum;
  let bytes_position = Option.value ~default:position bytes_position in
  lexbuf.bytes_offset <- bytes_position.Lexing.pos_cnum - lexbuf.bytes_pos;
  lexbuf.curr_bytes_bol <- bytes_position.Lexing.pos_bol

let set_filename lexbuf fname = lexbuf.filename <- fname

let from_gen ?bytes_per_char gen =
  let malformed = ref false in
  let refill buf pos len =
    let rec loop i =
      if !malformed then raise MalFormed;
      if i >= len then len
      else (
        match gen () with
          | Some c ->
              buf.(pos + i) <- c;
              loop (i + 1)
          | None -> i
          | exception MalFormed when i <> 0 ->
              malformed := true;
              i)
    in
    loop 0
  in
  create ?bytes_per_char refill

let from_int_array ?bytes_per_char a =
  from_gen ?bytes_per_char
    (Gen.init ~limit:(Array.length a) (fun i -> Uchar.of_int a.(i)))

let from_uchar_array ?(bytes_per_char = fun _ -> 1) a =
  let len = Array.length a in
  {
    (empty_lexbuf bytes_per_char) with
    buf = Array.init len (fun i -> a.(i));
    len;
    finished = true;
  }

let refill lexbuf =
  if lexbuf.len + chunk_size > Array.length lexbuf.buf then begin
    let s = lexbuf.start_pos in
    let s_bytes = lexbuf.start_bytes_pos in
    let ls = lexbuf.len - s in
    if ls + chunk_size <= Array.length lexbuf.buf then
      Array.blit lexbuf.buf s lexbuf.buf 0 ls
    else begin
      let newlen = (Array.length lexbuf.buf + chunk_size) * 2 in
      let newbuf = Array.make newlen dummy_uchar in
      Array.blit lexbuf.buf s newbuf 0 ls;
      lexbuf.buf <- newbuf
    end;
    lexbuf.len <- ls;
    lexbuf.offset <- lexbuf.offset + s;
    lexbuf.bytes_offset <- lexbuf.bytes_offset + s_bytes;
    lexbuf.pos <- lexbuf.pos - s;
    lexbuf.bytes_pos <- lexbuf.bytes_pos - s_bytes;
    lexbuf.marked_pos <- lexbuf.marked_pos - s;
    lexbuf.marked_bytes_pos <- lexbuf.marked_bytes_pos - s_bytes;
    lexbuf.start_pos <- 0;
    lexbuf.start_bytes_pos <- 0;
    (* Adjust tagged DFA memory cells: position cells (>= 0) are
       buffer-relative uchar indices and must be shifted by [s] after
       compaction. Value cells (<= -2) are left unchanged. Cells left over
       from an earlier token may be shifted too, which is harmless. *)
    for i = 0 to Array.length lexbuf.__private__mem - 1 do
      if lexbuf.__private__mem.(i) >= 0 then
        lexbuf.__private__mem.(i) <- lexbuf.__private__mem.(i) - s
    done
  end;
  let n = lexbuf.refill lexbuf.buf lexbuf.pos chunk_size in
  if n = 0 then lexbuf.finished <- true else lexbuf.len <- lexbuf.len + n

let new_line lexbuf =
  lexbuf.curr_line <- lexbuf.curr_line + 1;
  lexbuf.curr_bol <- lexbuf.pos + lexbuf.offset;
  lexbuf.curr_bytes_bol <- lexbuf.bytes_pos + lexbuf.bytes_offset

let[@inline always] next_aux some none lexbuf =
  if (not lexbuf.finished) && lexbuf.pos = lexbuf.len then refill lexbuf;
  if lexbuf.finished && lexbuf.pos = lexbuf.len then none
  else begin
    let ret = lexbuf.buf.(lexbuf.pos) in
    lexbuf.pos <- lexbuf.pos + 1;
    lexbuf.bytes_pos <- lexbuf.bytes_pos + lexbuf.bytes_per_char ret;
    if Uchar.equal ret nl_uchar then new_line lexbuf;
    some ret
  end

let next lexbuf = (next_aux [@inlined]) (fun x -> Some x) None lexbuf
let __private__next_int lexbuf = (next_aux [@inlined]) Uchar.to_int (-1) lexbuf

let mark lexbuf i =
  lexbuf.marked_pos <- lexbuf.pos;
  lexbuf.marked_bytes_pos <- lexbuf.bytes_pos;
  lexbuf.marked_bol <- lexbuf.curr_bol;
  lexbuf.marked_bytes_bol <- lexbuf.curr_bytes_bol;
  lexbuf.marked_line <- lexbuf.curr_line;
  lexbuf.marked_val <- i

let start lexbuf =
  lexbuf.start_pos <- lexbuf.pos;
  lexbuf.start_bytes_pos <- lexbuf.bytes_pos;
  lexbuf.start_bol <- lexbuf.curr_bol;
  lexbuf.start_bytes_bol <- lexbuf.curr_bytes_bol;
  lexbuf.start_line <- lexbuf.curr_line;
  mark lexbuf (-1)

let accept _lexbuf = true

let backtrack lexbuf =
  lexbuf.pos <- lexbuf.marked_pos;
  lexbuf.bytes_pos <- lexbuf.marked_bytes_pos;
  lexbuf.curr_bol <- lexbuf.marked_bol;
  lexbuf.curr_bytes_bol <- lexbuf.marked_bytes_bol;
  lexbuf.curr_line <- lexbuf.marked_line;
  lexbuf.marked_val

let rollback lexbuf =
  lexbuf.pos <- lexbuf.start_pos;
  lexbuf.bytes_pos <- lexbuf.start_bytes_pos;
  lexbuf.curr_bol <- lexbuf.start_bol;
  lexbuf.curr_bytes_bol <- lexbuf.start_bytes_bol;
  lexbuf.curr_line <- lexbuf.start_line

(* Tagged DFA memory cells for `as` bindings.
   Positions are stored as buffer-relative uchar indices (>= 0), converted
   to token-relative offsets on read by [__private__mem_pos]. Discriminator
   values are stored as -(v + 2), always <= -2. This range convention lets
   [refill] adjust only position cells (>= 0) when compacting the buffer. *)

(* Only grows the array. The cells keep the contents of the previous token:
   clearing them at each token costs more than the tag operations. *)
let __private__ensure_mem lexbuf n =
  if Array.length lexbuf.__private__mem < n then
    (* -1 is neither a position nor a value. Nothing relies on it: reused
       cells hold whatever earlier tokens left. *)
    lexbuf.__private__mem <- Array.make n (-1)

let __private__set_mem_pos lexbuf i = lexbuf.__private__mem.(i) <- lexbuf.pos

let __private__set_mem_prev_pos lexbuf i =
  lexbuf.__private__mem.(i) <- lexbuf.pos - 1

let __private__set_mem_value lexbuf i v =
  assert (v >= 0);
  lexbuf.__private__mem.(i) <- -(v + 2)

(* Copies the raw cell contents, preserving the position/value encoding. *)
let __private__copy_mem lexbuf dst src =
  lexbuf.__private__mem.(dst) <- lexbuf.__private__mem.(src)

(* Raw cell access, used by generated code to save a cell in a local
   variable when a parallel register move both reads and overwrites it. The
   value is opaque (position/value encoding preserved); [__private__mem_set]
   must only be given values obtained from [__private__mem_get]. *)
let __private__mem_get lexbuf i = lexbuf.__private__mem.(i)
let __private__mem_set lexbuf i v = lexbuf.__private__mem.(i) <- v

(* Returns position relative to token start, for use in sub_lexeme. *)
let __private__mem_pos lexbuf i = lexbuf.__private__mem.(i) - lexbuf.start_pos

(* Decodes the -(v + 2) encoding back to the original integer value. *)
let __private__mem_value lexbuf i = -(lexbuf.__private__mem.(i) + 2)
let __private__num_mem_cells lexbuf = Array.length lexbuf.__private__mem
let lexeme_start lexbuf = lexbuf.start_pos + lexbuf.offset
let lexeme_bytes_start lexbuf = lexbuf.start_bytes_pos + lexbuf.bytes_offset
let lexeme_end lexbuf = lexbuf.pos + lexbuf.offset
let lexeme_bytes_end lexbuf = lexbuf.bytes_pos + lexbuf.bytes_offset
let loc lexbuf = (lexbuf.start_pos + lexbuf.offset, lexbuf.pos + lexbuf.offset)

let bytes_loc lexbuf =
  ( lexbuf.start_bytes_pos + lexbuf.bytes_offset,
    lexbuf.bytes_pos + lexbuf.bytes_offset )

let lexeme_length lexbuf = lexbuf.pos - lexbuf.start_pos
let lexeme_bytes_length lexbuf = lexbuf.bytes_pos - lexbuf.start_bytes_pos

(* The index in [buf] of the code point at [pos] in the lexeme, once checked
   that [len] code points from there are in the lexeme. [buf] is the buffer
   of [lexbuf], read once by the caller: the range is also checked against
   that very array, so that the caller can read it without bounds checks
   whatever happens to [lexbuf] in between. *)
let sub_lexeme_offset name lexbuf buf pos len =
  let off = lexbuf.start_pos + pos in
  if
    pos < 0 || len < 0
    || pos > lexbuf.pos - lexbuf.start_pos - len
    || off > Array.length buf - len
  then invalid_arg name;
  off

let sub_lexeme lexbuf pos len =
  let buf = lexbuf.buf in
  let off = sub_lexeme_offset "Sedlexing.sub_lexeme" lexbuf buf pos len in
  Array.sub buf off len

type submatch = { lexbuf : lexbuf; pos : int; len : int }

let lexeme_of_submatch s = sub_lexeme s.lexbuf s.pos s.len

let lexeme lexbuf =
  Array.sub lexbuf.buf lexbuf.start_pos (lexbuf.pos - lexbuf.start_pos)

let lexeme_char lexbuf pos = lexbuf.buf.(lexbuf.start_pos + pos)

let lexing_position_start lexbuf =
  {
    Lexing.pos_fname = lexbuf.filename;
    pos_lnum = lexbuf.start_line;
    pos_cnum = lexbuf.start_pos + lexbuf.offset;
    pos_bol = lexbuf.start_bol;
  }

let lexing_position_curr lexbuf =
  {
    Lexing.pos_fname = lexbuf.filename;
    pos_lnum = lexbuf.curr_line;
    pos_cnum = lexbuf.pos + lexbuf.offset;
    pos_bol = lexbuf.curr_bol;
  }

let lexing_positions lexbuf =
  let start_p = lexing_position_start lexbuf
  and curr_p = lexing_position_curr lexbuf in
  (start_p, curr_p)

let lexing_bytes_position_start lexbuf =
  {
    Lexing.pos_fname = lexbuf.filename;
    pos_lnum = lexbuf.start_line;
    pos_cnum = lexbuf.start_bytes_pos + lexbuf.bytes_offset;
    pos_bol = lexbuf.start_bytes_bol;
  }

let lexing_bytes_position_curr lexbuf =
  {
    Lexing.pos_fname = lexbuf.filename;
    pos_lnum = lexbuf.curr_line;
    pos_cnum = lexbuf.bytes_pos + lexbuf.bytes_offset;
    pos_bol = lexbuf.curr_bytes_bol;
  }

let lexing_bytes_positions lexbuf =
  let start_p = lexing_bytes_position_start lexbuf
  and curr_p = lexing_bytes_position_curr lexbuf in
  (start_p, curr_p)

let with_tokenizer lexer' lexbuf =
  let lexer () =
    let token = lexer' lexbuf in
    let start_p, curr_p = lexing_positions lexbuf in
    (token, start_p, curr_p)
  in
  lexer

module Chan = struct
  exception Missing_input

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
      if len > 0 then Bytes.blit t.b t.pos t.b 0 len;
      let read = input t.ic t.b len (Bytes.length t.b - len) in
      t.len <- len + read;
      t.pos <- 0;
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

let make_from_channel ?bytes_per_char ic ~max_bytes_per_uchar
    ~min_bytes_per_uchar ~read_uchar =
  let t = Chan.create ic (chunk_size * max_bytes_per_uchar) in
  let malformed = ref false in
  let refill buf pos len =
    let rec loop i =
      if !malformed then raise MalFormed;
      if i = len then i
      else (
        match
          (* we refill our bytes buffer only if we haven't refilled any uchar yet. *)
          let can_refill = i = 0 in
          Chan.ensure_bytes_available t ~can_refill min_bytes_per_uchar;
          read_uchar ~can_refill t
        with
          | c ->
              buf.(pos + i) <- c;
              loop (i + 1)
          | exception MalFormed when i <> 0 ->
              malformed := true;
              i
          | exception Chan.Missing_input ->
              if i = 0 && Chan.available t > 0 then raise MalFormed;
              i)
    in
    loop 0
  in
  create ?bytes_per_char refill

module Latin1 = struct
  let from_gen s =
    from_gen ~bytes_per_char:(fun _ -> 1) (Gen.map Uchar.of_char s)

  let from_string s =
    let len = String.length s in
    {
      (empty_lexbuf (fun _ -> 1)) with
      buf = Array.init len (fun i -> Uchar.of_char s.[i]);
      len;
      finished = true;
    }

  let from_channel ic =
    make_from_channel ic
      ~bytes_per_char:(fun _ -> 1)
      ~min_bytes_per_uchar:1 ~max_bytes_per_uchar:1
      ~read_uchar:(fun ~can_refill:_ t ->
        let c = Chan.get t 0 in
        Chan.advance t 1;
        Uchar.of_char c)

  let[@inline] to_latin1 c =
    if Uchar.is_char c then Uchar.unsafe_to_char c
    else raise (InvalidCodepoint (Uchar.to_int c))

  let lexeme_char lexbuf pos = to_latin1 (lexeme_char lexbuf pos)

  let sub_lexeme lexbuf pos len =
    let buf = lexbuf.buf in
    let off =
      sub_lexeme_offset "Sedlexing.Latin1.sub_lexeme" lexbuf buf pos len
    in
    let s = Bytes.create len in
    for i = 0 to len - 1 do
      Bytes.unsafe_set s i (to_latin1 (Array.unsafe_get buf (off + i)))
    done;
    Bytes.unsafe_to_string s

  let lexeme lexbuf = sub_lexeme lexbuf 0 (lexbuf.pos - lexbuf.start_pos)
  let of_submatch s = sub_lexeme s.lexbuf s.pos s.len
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

    (* https://www.unicode.org/versions/corrigendum1.html *)
    (* U+0080..U+07FF — no surrogate check needed, below U+D800 *)
    let check_two n1 n2 =
      if n1 < 0xc2 || 0xdf < n1 then raise MalFormed;
      if n2 < 0x80 || 0xbf < n2 then raise MalFormed;
      if n2 lsr 6 != 0b10 then raise MalFormed;
      ((n1 land 0x1f) lsl 6) lor (n2 land 0x3f)

    let check_three n1 n2 n3 =
      if n1 = 0xe0 then (
        if n2 < 0xa0 || 0xbf < n2 then raise MalFormed;
        if n3 < 0x80 || 0xbf < n3 then raise MalFormed)
      else (
        if n1 < 0xe1 || 0xef < n1 then raise MalFormed;
        if n2 < 0x80 || 0xbf < n2 then raise MalFormed;
        if n3 < 0x80 || 0xbf < n3 then raise MalFormed);
      if n2 lsr 6 != 0b10 || n3 lsr 6 != 0b10 then raise MalFormed;
      let p =
        ((n1 land 0x0f) lsl 12) lor ((n2 land 0x3f) lsl 6) lor (n3 land 0x3f)
      in
      (* Reject UTF-16 surrogates (U+D800..U+DFFF) *)
      if p >= 0xd800 && p <= 0xdfff then raise MalFormed;
      p

    (* U+10000..U+10FFFF — no surrogate check needed, above U+DFFF *)
    let check_four n1 n2 n3 n4 =
      if n1 = 0xf0 then (
        if n2 < 0x90 || 0xbf < n2 then raise MalFormed;
        if n3 < 0x80 || 0xbf < n3 then raise MalFormed;
        if n4 < 0x80 || 0xbf < n4 then raise MalFormed)
      else if n1 = 0xf4 then (
        if n2 < 0x80 || 0x8f < n2 then raise MalFormed;
        if n3 < 0x80 || 0xbf < n3 then raise MalFormed;
        if n4 < 0x80 || 0xbf < n4 then raise MalFormed)
      else (
        if n1 < 0xf1 || 0xf3 < n1 then raise MalFormed;
        if n2 < 0x80 || 0xbf < n2 then raise MalFormed;
        if n3 < 0x80 || 0xbf < n3 then raise MalFormed;
        if n4 < 0x80 || 0xbf < n4 then raise MalFormed);
      if n2 lsr 6 != 0b10 || n3 lsr 6 != 0b10 || n4 lsr 6 != 0b10 then
        raise MalFormed;
      ((n1 land 0x07) lsl 18)
      lor ((n2 land 0x3f) lsl 12)
      lor ((n3 land 0x3f) lsl 6)
      lor (n4 land 0x3f)

    let next s i =
      let c1 = s.[i] in
      match width c1 with
        | 1 -> Char.code c1
        | 2 ->
            let n1 = Char.code c1 in
            let n2 = Char.code s.[i + 1] in
            check_two n1 n2
        | 3 ->
            let n1 = Char.code c1 in
            let n2 = Char.code s.[i + 1] in
            let n3 = Char.code s.[i + 2] in
            check_three n1 n2 n3
        | 4 ->
            let n1 = Char.code c1 in
            let n2 = Char.code s.[i + 1] in
            let n3 = Char.code s.[i + 2] in
            let n4 = Char.code s.[i + 3] in
            check_four n1 n2 n3 n4
        | _ -> assert false

    let gen_from_char_gen s =
      let next_or_fail () =
        match Gen.next s with None -> raise MalFormed | Some x -> Char.code x
      in
      fun () ->
        Gen.next s >>| fun c1 ->
        match width c1 with
          | 1 -> Uchar.of_char c1
          | 2 ->
              let n1 = Char.code c1 in
              let n2 = next_or_fail () in
              Uchar.of_int (check_two n1 n2)
          | 3 ->
              let n1 = Char.code c1 in
              let n2 = next_or_fail () in
              let n3 = next_or_fail () in
              Uchar.of_int (check_three n1 n2 n3)
          | 4 ->
              let n1 = Char.code c1 in
              let n2 = next_or_fail () in
              let n3 = next_or_fail () in
              let n4 = next_or_fail () in
              Uchar.of_int (check_four n1 n2 n3 n4)
          | _ -> raise MalFormed
  end

  let from_channel ic =
    make_from_channel ic ~bytes_per_char:Uchar.utf_8_byte_length
      ~min_bytes_per_uchar:1 ~max_bytes_per_uchar:4
      ~read_uchar:(fun ~can_refill t ->
        let w = Helper.width (Chan.get t 0) in
        Chan.ensure_bytes_available t ~can_refill w;
        let c =
          Helper.next (Bytes.unsafe_to_string (Chan.raw_buf t)) (Chan.raw_pos t)
        in
        Chan.advance t w;
        Uchar.of_int c)

  let from_gen s =
    from_gen ~bytes_per_char:Uchar.utf_8_byte_length
      (Helper.gen_from_char_gen s)

  let from_string s =
    from_gen (Gen.init ~limit:(String.length s) (fun i -> String.get s i))

  (* A first pass computes the size of the result, which is then written in
     place: no intermediate buffer. *)
  let sub_lexeme lexbuf pos len =
    let buf = lexbuf.buf in
    let off =
      sub_lexeme_offset "Sedlexing.Utf8.sub_lexeme" lexbuf buf pos len
    in
    let size = ref 0 in
    for i = off to off + len - 1 do
      size := !size + Uchar.utf_8_byte_length (Array.unsafe_get buf i)
    done;
    let s = Bytes.create !size in
    if !size = len then
      (* ASCII only, the common case: a plain copy is about 15% faster on
         lexemes of a few dozen characters. *)
      for i = 0 to len - 1 do
        Bytes.unsafe_set s i
          (Uchar.unsafe_to_char (Array.unsafe_get buf (off + i)))
      done
    else begin
      let j = ref 0 in
      for i = off to off + len - 1 do
        j := !j + Bytes.set_utf_8_uchar s !j (Array.unsafe_get buf i)
      done;
      (* the buffer was modified since the size was computed *)
      if !j <> !size then invalid_arg "Sedlexing.Utf8.sub_lexeme"
    end;
    Bytes.unsafe_to_string s

  let lexeme lexbuf = sub_lexeme lexbuf 0 (lexbuf.pos - lexbuf.start_pos)
  let of_submatch s = sub_lexeme s.lexbuf s.pos s.len
end

module Utf16 = struct
  type byte_order = Little_endian | Big_endian

  module Helper = struct
    (* http://www.ietf.org/rfc/rfc2781.txt *)

    let number_of_pair bo c1 c2 =
      match bo with
        | Little_endian -> (c2 lsl 8) + c1
        | Big_endian -> (c1 lsl 8) + c2

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

    let gen_from_char_gen opt_bo s =
      let next_or_fail () =
        match Gen.next s with None -> raise MalFormed | Some x -> Char.code x
      in
      let bo = ref opt_bo in
      fun () ->
        Gen.next s >>| fun c1 ->
        let n1 = Char.code c1 in
        let n2 = next_or_fail () in
        let o = get_bo bo n1 n2 in
        let w1 = number_of_pair o n1 n2 in
        if w1 = 0xfffe then raise (InvalidCodepoint w1);
        if w1 < 0xd800 || 0xdfff < w1 then Uchar.of_int w1
        else if w1 <= 0xdbff then (
          let n3 = next_or_fail () in
          let n4 = next_or_fail () in
          let w2 = number_of_pair o n3 n4 in
          if w2 < 0xdc00 || w2 > 0xdfff then raise MalFormed;
          let upper10 = (w1 land 0x3ff) lsl 10 and lower10 = w2 land 0x3ff in
          Uchar.of_int (0x10000 + upper10 + lower10))
        else raise MalFormed
  end

  let from_channel ic opt_bo =
    let bo = ref opt_bo in
    make_from_channel ic ~bytes_per_char:Uchar.utf_16_byte_length
      ~min_bytes_per_uchar:2 ~max_bytes_per_uchar:4
      ~read_uchar:(fun ~can_refill t ->
        let n1 = Char.code (Chan.get t 0) in
        let n2 = Char.code (Chan.get t 1) in
        let o = Helper.get_bo bo n1 n2 in
        let w1 = Helper.number_of_pair o n1 n2 in
        if w1 = 0xfffe then raise (InvalidCodepoint w1);
        if w1 < 0xd800 || 0xdfff < w1 then (
          Chan.advance t 2;
          Uchar.of_int w1)
        else if w1 <= 0xdbff then (
          Chan.ensure_bytes_available t ~can_refill 4;
          let n3 = Char.code (Chan.get t 2) in
          let n4 = Char.code (Chan.get t 3) in
          let w2 = Helper.number_of_pair o n3 n4 in
          if w2 < 0xdc00 || w2 > 0xdfff then raise MalFormed;
          let upper10 = (w1 land 0x3ff) lsl 10 and lower10 = w2 land 0x3ff in
          Chan.advance t 4;
          Uchar.of_int (0x10000 + upper10 + lower10))
        else raise MalFormed)

  let from_gen s opt_bo =
    from_gen ~bytes_per_char:Uchar.utf_16_byte_length
      (Helper.gen_from_char_gen opt_bo s)

  let from_string s =
    from_gen (Gen.init ~limit:(String.length s) (fun i -> String.get s i))

  (* As for UTF-8: the size first, then the result written in place. The
     encoding loop is written once for each byte order, so that the encoder
     is inlined in it. *)
  let sub_lexeme lb pos len bo bom =
    let buf = lb.buf in
    let off = sub_lexeme_offset "Sedlexing.Utf16.sub_lexeme" lb buf pos len in
    let size = ref (if bom then 2 else 0) in
    for i = off to off + len - 1 do
      size := !size + Uchar.utf_16_byte_length (Array.unsafe_get buf i)
    done;
    let s = Bytes.create !size in
    let written =
      match bo with
        | Big_endian ->
            let j =
              ref (if bom then Bytes.set_utf_16be_uchar s 0 Uchar.bom else 0)
            in
            for i = off to off + len - 1 do
              j := !j + Bytes.set_utf_16be_uchar s !j (Array.unsafe_get buf i)
            done;
            !j
        | Little_endian ->
            let j =
              ref (if bom then Bytes.set_utf_16le_uchar s 0 Uchar.bom else 0)
            in
            for i = off to off + len - 1 do
              j := !j + Bytes.set_utf_16le_uchar s !j (Array.unsafe_get buf i)
            done;
            !j
    in
    (* the buffer was modified since the size was computed *)
    if written <> !size then invalid_arg "Sedlexing.Utf16.sub_lexeme";
    Bytes.unsafe_to_string s

  let lexeme lb bo bom = sub_lexeme lb 0 (lb.pos - lb.start_pos) bo bom
  let of_submatch s bo bom = sub_lexeme s.lexbuf s.pos s.len bo bom
end
