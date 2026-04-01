(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

(** {2 Regexp combinators}

    Regular expressions are built from combinators and compiled to a DFA. *)

(** Abstract type of regular expressions. *)
type regexp

(** [chars cset] matches a single code point in [cset]. *)
val chars : Sedlex_cset.t -> regexp

(** [seq r1 r2] matches [r1] followed by [r2] (concatenation). *)
val seq : regexp -> regexp -> regexp

(** [alt r1 r2] matches [r1] or [r2] (alternation). When both operands are
    simple [chars] regexps, their character sets are merged into one. *)
val alt : regexp -> regexp -> regexp

(** [rep r] matches zero or more repetitions of [r] (Kleene star). *)
val rep : regexp -> regexp

(** [plus r] matches one or more repetitions of [r]. *)
val plus : regexp -> regexp

(** The empty regexp — matches the empty string (epsilon). *)
val eps : regexp

(** If the argument is a single [chars] regexp, returns a regexp which matches
    the complement set. Otherwise returns [None]. *)
val compl : regexp -> regexp option

(** If each argument is a single [chars] regexp, returns a regexp which matches
    the set (arg1 - arg2). Otherwise returns [None]. *)
val subtract : regexp -> regexp -> regexp option

(** If each argument is a single [chars] regexp, returns a regexp which matches
    the intersection set. Otherwise returns [None]. *)
val intersection : regexp -> regexp -> regexp option

(** {2 Tagged DFA for [as] bindings}

    Named sub-match bindings (e.g. [Star any as x]) are implemented using tagged
    transitions in the DFA. Each [as] binding introduces up to two tags that
    record the start and end positions of the sub-match in the lexbuf's memory
    cells at runtime. When one boundary can be computed from a known offset,
    only one tag is needed ([bind_start_only] / [bind_end_only]); when both
    boundaries are known, no tags are needed at all.

    Or-patterns [(p1 as x) | (p2 as x)] additionally use discriminator cells:
    integer values that record which branch was taken, so the PPX can extract
    the correct positions at match time. *)

(** Tag operations emitted on DFA transitions. *)
type tag_op =
  | Set_position of int
      (** [Set_position i]: record the current lexbuf position in memory cell
          [i]. *)
  | Set_value of int * int
      (** [Set_value (cell, v)]: record integer [v] in memory cell [cell] (used
          for or-pattern discriminators). *)

(** [bind r] wraps [r] with start/end tag epsilon nodes. Returns
    [(wrapped_regexp, start_tag, end_tag)] where [start_tag] and [end_tag] are
    the allocated memory cell indices. *)
val bind : regexp -> regexp * int * int

(** Allocate a fresh memory cell for an or-pattern discriminator. *)
val new_disc_cell : unit -> int

(** [bind_disc r cell value] appends an epsilon node that sets [cell] to [value]
    after [r] matches. Used to tag each branch of an or-pattern so the PPX can
    tell which branch matched. *)
val bind_disc : regexp -> int -> int -> regexp

(** Like [bind], but only wraps with a start tag (no end tag). Used when the end
    position can be computed from a known offset. *)
val bind_start_only : regexp -> regexp * int

(** Like [bind], but only wraps with an end tag (no start tag). Used when the
    start position can be computed from a known offset. *)
val bind_end_only : regexp -> regexp * int

(** [tag_end tag_id r] wraps [r] with a pre-allocated end tag: the NFA becomes
    [r → [tag_id] → successor]. Unlike [bind_end_only], does not allocate a
    fresh tag — uses the given [tag_id]. *)
val tag_end : int -> regexp -> regexp

(** Reset the tag counter. Called before compiling each [match%sedlex] block. *)
val reset_tags : unit -> unit

(** {2 DFA compilation} *)

type dfa_state = {
  trans : (Sedlex_cset.t * int * tag_op list) array;
      (** Each transition: (character set, target state, tag operations to
          execute when this transition fires). *)
  finals : bool array;
      (** [finals.(i)] is [true] if this state is accepting for rule [i]. *)
}

(** DFA states, indexed by state number. State 0 is the initial state. *)
type dfa = dfa_state array

(** Result of [compile]. *)
type compiled = {
  dfa : dfa;
  init_tags : tag_op list;
      (** Tag operations to execute before entering the DFA (from epsilon
          closure of the initial NFA nodes). *)
  num_tags : int;
      (** Total number of memory cells needed at runtime. When [num_tags = 0],
          no memory is allocated (pattern has no [as] bindings). *)
}

(** [compile rules] determinizes the NFA for an array of regexp rules using
    subset construction. Returns the DFA, initial tag operations, and the total
    number of memory cells needed for [as] bindings. State 0 is always the
    initial state. *)
val compile : regexp array -> compiled

(** [optimize ~live compiled] eliminates dead tags and remaps live ones to a
    dense range. [live] is a list of tag IDs that are referenced in the
    generated code. Tags not in [live] are stripped from all DFA transitions and
    [init_tags], and [num_tags] is reduced accordingly. Returns the optimized
    compiled result and a mapping array [old_tag → new_tag] ([-1] for dead tags,
    identity when nothing changed). *)
val optimize : live:int list -> compiled -> compiled * int array

(** [dfa_to_dot dfa] returns a Graphviz DOT representation of the DFA, including
    state labels, accepting state markers, transition character sets, and tag
    operations on edges. *)
val dfa_to_dot : dfa -> string
