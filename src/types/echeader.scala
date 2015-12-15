// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._

final case class echeader(var ec: Int) extends DeepCopyable[echeader] {
  override def deepCopy(): echeader = echeader(ec)
}

object echeader {
  /**
   * Functions for constructors
   */
  def echdr(ec: Int) : echeader = {
    echeader(ec)
  }

  def uninit = echdr(0)

  implicit object Randomizer extends helpers.scala.Randomizer[echeader] {
    def random() : echeader = echeader(helpers.scala.Random[Int])
  }
}
