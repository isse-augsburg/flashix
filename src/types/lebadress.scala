// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._

final case class lebadress(vol: Byte, leb: Int) {}

object lebadress {
  /**
   * Functions for constructors
   */
  def ×(vol: Byte, leb: Int) : lebadress = {
    lebadress(vol, leb)
  }

  def uninit = ×(0.toByte, 0)

  implicit object Randomizer extends helpers.scala.Randomizer[lebadress] {
    def random() : lebadress = lebadress(helpers.scala.Random[Byte], helpers.scala.Random[Int])
  }
}
