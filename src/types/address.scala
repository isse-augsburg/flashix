package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

final case class address(lnum: Int, pos: Int, size: Int) {}

object address {
  /**
   * Functions for constructors
   */
  def at(lnum: Int, pos: Int, size: Int): address = {
    address(lnum, pos, size)
  }

  def uninit = at(0, 0, 0)

  implicit object Randomizer extends helpers.scala.Randomizer[address] {
    override def random(): address = address(helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[Int])
  }
}
