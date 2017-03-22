// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

final case class vidheader(vol: Byte, leb: Int, sqn: Int, size: Int, checksum: Int) {}

object vidheader {
  /**
   * Functions for constructors
   */
  def vidhdr(vol: Byte, leb: Int, sqn: Int, size: Int, checksum: Int): vidheader = {
    vidheader(vol, leb, sqn, size, checksum)
  }

  def uninit = vidhdr(0.toByte, 0, 0, 0, 0)

  implicit object Randomizer extends helpers.scala.Randomizer[vidheader] {
    override def random(): vidheader = vidheader(helpers.scala.Random[Byte], helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[Int])
  }
}
