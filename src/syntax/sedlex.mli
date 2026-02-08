(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

type regexp

val chars : Sedlex_cset.t -> regexp
val seq : regexp -> regexp -> regexp
val alt : regexp -> regexp -> regexp
val rep : regexp -> regexp
val plus : regexp -> regexp
val eps : regexp
val compl : regexp -> regexp option

(* If the argument is a single [chars] regexp, returns a regexp
   which matches the complement set.  Otherwise returns [None]. *)
val subtract : regexp -> regexp -> regexp option

(* If each argument is a single [chars] regexp, returns a regexp
   which matches the set (arg1 - arg2).  Otherwise returns [None]. *)
val intersection : regexp -> regexp -> regexp option
(* If each argument is a single [chars] regexp, returns a regexp
   which matches the intersection set.  Otherwise returns [None]. *)

type tag_op = Set_position of int | Set_value of int * int | Set_prev of int

val bind : regexp -> regexp * int * int
val new_disc_cell : unit -> int
val bind_disc : regexp -> int -> int -> regexp
val bind_start_only : regexp -> regexp * int
val bind_end_only : regexp -> regexp * int
val reset_tags : unit -> unit

type dfa_state = {
  trans : (Sedlex_cset.t * int * tag_op list) array;
  finals : bool array;
}

type dfa = dfa_state array

type compiled = {
  dfa : dfa;
  init_tags : tag_op list;
  num_tags : int;
  tag_map : int array;
}

val compile : regexp array -> compiled
val dfa_to_dot : dfa -> string
