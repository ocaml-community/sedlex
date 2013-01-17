(* The package sedlex is released under the terms of an MIT-like license. *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

type regexp

val chars: Cset.t -> regexp
val seq: regexp -> regexp -> regexp
val alt: regexp -> regexp -> regexp
val rep: regexp -> regexp
val plus: regexp -> regexp
val eps: regexp

val compile: regexp array -> ((Cset.t * int) array * bool array) array
