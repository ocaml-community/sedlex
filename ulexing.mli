(** Runtime support for Ulex-generated lexers *)

type lexbuf
  (** Buffer used by lexers. *)

exception Error
  (** Raised by a lexer when it cannot parse a token from the lexbuf. *)

(** {6 Clients interface} *)

val create: (int array -> int -> int -> int) -> lexbuf
  (** This function creates a generic lexbuf. The argument [f] is
    called by the lexers to refill the internal buffer.
    A call like [f buf pos len] means that [f] has to store at most
    [len] characters in [buf] starting at position [pos].
    The call must return the number of characters actually stored
    into the buffer. A return value of [0] indicate the end
    of the input stream.
  *)

val from_stream: int Stream.t -> lexbuf
val from_int_array: int array -> lexbuf


val from_latin1_stream: char Stream.t -> lexbuf
val from_latin1_channel: in_channel -> lexbuf
val from_latin1_string: string -> lexbuf

val from_utf8_stream: char Stream.t -> lexbuf
val from_utf8_channel: in_channel -> lexbuf
val from_utf8_string: string -> lexbuf


(** {6 Actions interface} *)

(** These functions can be used by the actions in the lexers. *)

val lexeme_start: lexbuf -> int
val lexeme_end: lexbuf -> int

val loc: lexbuf -> int * int

val lexeme_length: lexbuf -> int


val sub_lexeme: lexbuf -> int -> int -> int array
val lexeme: lexbuf -> int array
val lexeme_char: lexbuf -> int -> int

val latin1_sub_lexeme: lexbuf -> int -> int -> string
val latin1_lexeme: lexbuf -> string
val latin1_lexeme_char: lexbuf -> int -> char
 
val utf8_sub_lexeme: lexbuf -> int -> int -> string
val utf8_lexeme: lexbuf -> string

(** {6 Internal interface} *)

(** These functions are used internally by the lexers. *)

val start: lexbuf -> unit
val next: lexbuf -> int
val mark: lexbuf -> int -> unit
val backtrack: lexbuf -> int
