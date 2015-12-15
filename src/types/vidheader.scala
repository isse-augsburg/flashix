package types

import helpers.scala._

final case class vidheader(var vol: Byte, var leb: Int, var sqn: Int, var size: Int, var checksum: Int) extends DeepCopyable[vidheader] {
  override def deepCopy(): vidheader = vidheader(vol, leb, sqn, size, checksum)
}

object vidheader {
  /**
   * Functions for constructors
   */
  def vidhdr(vol: Byte, leb: Int, sqn: Int, size: Int, checksum: Int) : vidheader = {
    vidheader(vol, leb, sqn, size, checksum)
  }

  def uninit = vidhdr(0.toByte, 0, 0, 0, 0)

  implicit object Randomizer extends helpers.scala.Randomizer[vidheader] {
    def random() : vidheader = vidheader(helpers.scala.Random[Byte], helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[Int])
  }
}
