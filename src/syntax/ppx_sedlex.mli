(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

open Sedlex_compiler

(** PPX rewriter for sedlex.

    Transforms [match%sedlex lexbuf with ...] into DFA-based lexer code.
    Registration with ppxlib is done via side effects at module initialization.
*)

(** {2 Internals exposed for the test PPX}

    The values below are not part of the public API. They are exposed so that
    [ppx_sedlex_test] can invoke the code generator in isolation and inspect the
    generated AST and DFA. *)

(** Clear all internal tables (partitions, lookup tables). Must be called
    between independent compilations in tests. *)
val reset_state : unit -> unit

(** [handle_sedlex_match match_expr] compiles a [match%sedlex lexbuf with ...]
    expression into generated DFA code using the built-in regexp environment.
    Returns the generated expression and the DFA automaton. *)
val handle_sedlex_match :
  Ppxlib.Parsetree.expression -> Ppxlib.Parsetree.expression * Sedlex.dfa

(** [map_expression expr] applies the sedlex mapper to [expr], processing any
    [[%sedlex]] or [[%sedlex.regexp?]] extensions it contains. *)
val map_expression : Ppxlib.Parsetree.expression -> Ppxlib.Parsetree.expression
