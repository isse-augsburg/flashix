package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

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
