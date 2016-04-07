// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

final case class recoveryentry(pnum: Int, sqn: Int) {}

object recoveryentry {
  /**
   * Functions for constructors
   */
  def recovery_entry(pnum: Int, sqn: Int): recoveryentry = {
    recoveryentry(pnum, sqn)
  }

  def uninit = recovery_entry(0, 0)

  implicit object Randomizer extends helpers.scala.Randomizer[recoveryentry] {
    override def random(): recoveryentry = recoveryentry(helpers.scala.Random[Int], helpers.scala.Random[Int])
  }
}
