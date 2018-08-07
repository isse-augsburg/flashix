// Flashix: a verified file system for flash memory
// (c) 2015-2018 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._

final case class erasequeueentry(pnum: Int, lebref: lebref) {}

object erasequeueentry {
  /**
   * Functions for constructors
   */
  def eq_entry(pnum: Int, lebref: lebref): erasequeueentry = {
    erasequeueentry(pnum, lebref)
  }

  def uninit = eq_entry(0, types.lebref.uninit)

  implicit object Randomizer extends helpers.scala.Randomizer[erasequeueentry] {
    override def random(): erasequeueentry = erasequeueentry(helpers.scala.Random[Int], helpers.scala.Random[lebref])
  }
}
