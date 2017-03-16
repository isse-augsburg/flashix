package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

object file_mode extends helpers.scala.Random.Enumeration {
  type file_mode = Value
  val MODE_R, MODE_W, MODE_RW = Value

  def uninit = MODE_R

  implicit object file_modeRandomizer extends helpers.scala.Randomizer[file_mode.file_mode] {
    override def random(): file_mode.file_mode = file_mode.random()
  }
}
