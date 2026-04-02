(** NFA/DFA oracle for testing sedlex's tagged DFA compilation. *)

(** {2 Regexp description} *)

type desc =
  | Lit of char
  | Seq of desc * desc
  | Alt of desc * desc
  | Star of desc
  | Plus of desc
  | Opt of desc
  | Rep of desc * int * int
  | Class of char * char
  | Compl of desc
  | Sub of desc * desc
  | Inter of desc * desc
  | Bind of string * desc

val lit : char -> desc
val cls : char -> char -> desc
val seq : desc -> desc -> desc
val alt : desc -> desc -> desc
val star : desc -> desc
val plus : desc -> desc
val opt : desc -> desc
val rep : desc -> int -> int -> desc
val compl : desc -> desc
val sub : desc -> desc -> desc
val inter : desc -> desc -> desc
val capture : string -> desc -> desc
val ( ^. ) : desc -> desc -> desc
val show : desc -> string

(** {2 Simulation results} *)

type binding = { name : string; start_offset : int; end_offset : int }
type result = { rule : int; length : int; bindings : binding list }

(** {2 Oracle} *)

(** [oracle rules input] compiles each rule (a [unit -> desc] thunk), runs both
    NFA and DFA simulators, and prints the result. Prints [FIXME] when NFA and
    DFA disagree. *)
val oracle : (unit -> desc) array -> string -> unit

(** {2 QCheck support} *)

(** [qcheck_oracle ~check_tags (descs, input)] returns [true] if NFA and DFA
    agree on acceptance (and bindings when [check_tags] is true). *)
val qcheck_oracle : check_tags:bool -> desc array * string -> bool

val gen_desc : desc QCheck2.Gen.t
val gen_input : string QCheck2.Gen.t
val print_case : desc array * string -> string
