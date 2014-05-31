module Categories : sig

  val cc : Cset.t
  val cf : Cset.t
  val cn : Cset.t
  val co : Cset.t
  val cs : Cset.t
  val ll : Cset.t
  val lm : Cset.t
  val lo : Cset.t
  val lt : Cset.t
  val lu : Cset.t
  val mc : Cset.t
  val me : Cset.t
  val mn : Cset.t
  val nd : Cset.t
  val nl : Cset.t
  val no : Cset.t
  val pc : Cset.t
  val pd : Cset.t
  val pe : Cset.t
  val pf : Cset.t
  val pi : Cset.t
  val po : Cset.t
  val ps : Cset.t
  val sc : Cset.t
  val sk : Cset.t
  val sm : Cset.t
  val so : Cset.t
  val zl : Cset.t
  val zp : Cset.t
  val zs : Cset.t

end

module Properties : sig

  val alphabetic       : Cset.t
  val ascii_hex_digit  : Cset.t
  val hex_digit        : Cset.t
  val id_continue      : Cset.t
  val id_start         : Cset.t
  val lowercase        : Cset.t
  val math             : Cset.t
  val other_alphabetic : Cset.t
  val other_lowercase  : Cset.t
  val other_math       : Cset.t
  val other_uppercase  : Cset.t
  val uppercase        : Cset.t
  val white_space      : Cset.t
  val xid_continue     : Cset.t
  val xid_start        : Cset.t

end
