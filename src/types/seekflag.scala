// Flashix: a verified file system for flash memory
// (c) 2015-2018 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._

object seekflag extends helpers.scala.Random.Enumeration {
  type seekflag = Value
  val SEEK_SET, SEEK_CUR, SEEK_END = Value

  def uninit = SEEK_CUR

  implicit object seekflagRandomizer extends helpers.scala.Randomizer[seekflag.seekflag] {
    override def random(): seekflag.seekflag = seekflag.random()
  }
}
