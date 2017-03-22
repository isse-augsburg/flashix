// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

final case class binheap(var ar: key_array, var size: Int) extends DeepCopyable[binheap] {
  override def deepCopy(): binheap = binheap(ar.deepCopy, size)

  def := (other: binheap) {
    ar = other.ar
    size = other.size
  }
}

object binheap {
  /**
   * Functions for constructors
   */
  def bin_heap(ar: key_array, size: Int): binheap = {
    binheap(ar, size)
  }

  def uninit = bin_heap(new key_array(), 0)

  implicit object Randomizer extends helpers.scala.Randomizer[binheap] {
    override def random(): binheap = binheap(helpers.scala.Random[key_array], helpers.scala.Random[Int])
  }
}
