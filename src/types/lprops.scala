// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types.lpropflags.lpropflags

final case class lprops(var ref_size: Int, var size: Int, var flags: lpropflags, var gcheapidx: Int) extends DeepCopyable[lprops] {
  override def deepCopy(): lprops = lprops(ref_size, size, flags, gcheapidx)

  def := (other: lprops) {
    ref_size = other.ref_size
    size = other.size
    flags = other.flags
    gcheapidx = other.gcheapidx
  }
}

object lprops {
  /**
   * Functions for constructors
   */
  def mklp(ref_size: Int, size: Int, flags: lpropflags, gcheapidx: Int): lprops = {
    lprops(ref_size, size, flags, gcheapidx)
  }

  def uninit = mklp(0, 0, types.lpropflags.uninit, 0)

  implicit object Randomizer extends helpers.scala.Randomizer[lprops] {
    override def random(): lprops = lprops(helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[lpropflags], helpers.scala.Random[Int])
  }
}
