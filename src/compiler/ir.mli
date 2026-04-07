(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

(** Intermediate representation for sedlex patterns.

    This IR captures the regexp structure including named capture annotations
    ([as] bindings) but {b before} tag allocation, fixed-length optimization, or
    discriminator handling. The compiler processes this IR to produce a compiled
    DFA with binding extraction information.

    The IR does not depend on ppxlib and can be constructed directly in tests.
*)

(** A pattern with named captures. *)
type t =
  | Chars of Cset.t  (** Match a single code point in the given set. *)
  | Seq of t list
      (** Sequence (concatenation). Invariant: length >= 2. Use {!seq} smart
          constructor. *)
  | Alt of t list
      (** Alternation. Invariant: length >= 2. Use {!alt} smart constructor
          which flattens nested [Alt]s. *)
  | Star of t  (** Kleene star (zero or more). *)
  | Plus of t  (** One or more. *)
  | Rep of t * int * int
      (** [Rep (r, n, m)]: between [n] and [m] repetitions. *)
  | Eps  (** Empty string (epsilon). *)
  | Capture of string * t
      (** Named capture: [Capture (name, inner)] wraps [inner] with an [as]
          binding. The compiler decides tag allocation strategy. *)

(** {2 Smart constructors} *)

val chars : Cset.t -> t
val seq : t -> t -> t
val alt : t -> t -> t
val star : t -> t
val plus : t -> t
val rep : t -> int -> int -> t
val eps : t
val capture : string -> t -> t

(** {2 Analysis} *)

(** [fixed_length t] returns [Some n] if [t] always matches exactly [n] code
    points, or [None] if the length is variable. *)
val fixed_length : t -> int option

module SSet : Set.S with type elt = string

(** [capture_names t] returns the set of capture names in [t]. *)
val capture_names : t -> SSet.t

(** {2 Validation}

    [validate t] checks structural constraints:
    - [Capture] not inside [Star], [Plus], or [Rep]
    - All branches of [Alt] bind the same capture names

    Returns [Ok ()] or [Error msg]. *)
val validate : t -> (unit, string) result

(** {2 Pretty-printing} *)

val pp : Format.formatter -> t -> unit
val show : t -> string
