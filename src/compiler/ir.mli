(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2026, Hugo Heuzard                                           *)

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

(** {2 Smart constructors}

    Constructors that enforce structural invariants return [result].
    - {!alt} checks name consistency across branches.
    - {!capture} checks for shadowed inner bindings.
    - {!seq} rejects duplicate capture names across elements.
    - {!star}, {!plus}, {!rep} reject inner captures. *)

val chars : Cset.t -> t
val seq : t -> t -> (t, string) result
val alt : t -> t -> (t, string) result
val star : t -> (t, string) result
val plus : t -> (t, string) result
val rep : t -> int -> int -> (t, string) result
val eps : t
val capture : string -> t -> (t, string) result

(** [reject_captures ctx t] returns [Ok t] if [t] contains no [Capture] nodes,
    or [Error msg] mentioning [ctx] otherwise. *)
val reject_captures : string -> t -> (t, string) result

(** {2 Analysis} *)

module SSet : Set.S with type elt = string

(** [capture_names t] returns the set of capture names in [t]. *)
val capture_names : t -> SSet.t

(** [fixed_length t] returns [Some n] if [t] always matches exactly [n] code
    points, or [None] if the length is variable. *)
val fixed_length : t -> int option

(** {2 Invariant checking}

    All structural constraints are enforced by the smart constructors.
    [check_invariant] asserts these hold. Use for debugging. *)
val check_invariant : t -> unit

(** {2 Pretty-printing} *)

val pp : Format.formatter -> t -> unit
val show : t -> string
