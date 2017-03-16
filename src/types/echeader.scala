package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

final case class echeader(ec: Int) {}

object echeader {
  /**
   * Functions for constructors
   */
  def echdr(ec: Int): echeader = {
    echeader(ec)
  }

  def uninit = echdr(0)

  implicit object Randomizer extends helpers.scala.Randomizer[echeader] {
    override def random(): echeader = echeader(helpers.scala.Random[Int])
  }
}
