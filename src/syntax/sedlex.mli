(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

type regexp

val chars: Cset.t -> regexp
val seq: regexp -> regexp -> regexp
val alt: regexp -> regexp -> regexp
val rep: regexp -> regexp
val plus: regexp -> regexp
val eps: regexp

val compl: regexp -> regexp option
val subtract: regexp -> regexp -> regexp option
val intersection: regexp -> regexp -> regexp option
   (* If the argument is a single [chars] regexp, returns a regexp
      which matches the complement set.  Otherwise returns [None]. *)


val compile: regexp array -> ((Cset.t * int) array * bool array) array
