(** Oracle for testing sedlex's tagged DFA compilation.

    Two independent simulators are compared:
    - ocamllex: vendored ocamllex DFA compiler (independent TDFA)
    - DFA: sedlex's own determinized tagged DFA *)

open Sedlex_compiler

(** {2 Convenience builders for Ir.t}

    Thin wrappers around {!Ir} smart constructors that raise on error instead of
    returning [result]. *)

val lit : char -> Ir.t
val cls : char -> char -> Ir.t
val seq : Ir.t -> Ir.t -> Ir.t
val alt : Ir.t -> Ir.t -> Ir.t
val star : Ir.t -> Ir.t
val plus : Ir.t -> Ir.t
val opt : Ir.t -> Ir.t
val rep : Ir.t -> int -> int -> Ir.t
val compl : Ir.t -> Ir.t
val sub : Ir.t -> Ir.t -> Ir.t
val inter : Ir.t -> Ir.t -> Ir.t
val capture : string -> Ir.t -> Ir.t
val ( ^. ) : Ir.t -> Ir.t -> Ir.t

(** {2 Oracle} *)

(** [check rules input] compiles each rule, runs the ocamllex DFA and sedlex
    DFA, and returns [true] iff both agree exactly. *)
val check : Ir.t array -> string -> bool

(** [oracle rules input] is like {!check} but also prints the result. Prints
    [ERROR] when the two disagree. *)
val oracle : Ir.t array -> string -> unit

(** {2 QCheck support} *)

val gen_ir : Ir.t QCheck2.Gen.t
val gen_input : string QCheck2.Gen.t
val print_case : Ir.t array * string -> string
