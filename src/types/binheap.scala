package types

import helpers.scala._

final case class binheap(ar: key_array, size: Int) extends DeepCopyable[binheap] {
  override def deepCopy(): binheap = binheap(ar.deepCopy, size)
}

object binheap {
  /**
   * Functions for constructors
   */
  def bin_heap(ar: key_array, size: Int) : binheap = {
    binheap(ar, size)
  }

  def uninit = bin_heap(new key_array(), 0)

  implicit object Randomizer extends helpers.scala.Randomizer[binheap] {
    def random() : binheap = binheap(helpers.scala.Random[key_array], helpers.scala.Random[Int])
  }
}
