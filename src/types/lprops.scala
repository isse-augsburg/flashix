package types

import helpers.scala._
import types.lpropflags.lpropflags

final case class lprops(var ref_size: Int, var size: Int, var flags: lpropflags, var gcheapidx: Int) extends DeepCopyable[lprops] {
  override def deepCopy(): lprops = lprops(ref_size, size, flags, gcheapidx)
}

object lprops {
  /**
   * Functions for constructors
   */
  def mklp(ref_size: Int, size: Int, flags: lpropflags, gcheapidx: Int) : lprops = {
    lprops(ref_size, size, flags, gcheapidx)
  }

  def uninit = mklp(0, 0, lpropflags.uninit, 0)

  implicit object Randomizer extends helpers.scala.Randomizer[lprops] {
    def random() : lprops = lprops(helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[lpropflags], helpers.scala.Random[Int])
  }
}
