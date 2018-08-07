// Flashix: a verified file system for flash memory
// (c) 2015-2018 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._

sealed abstract class key {
  def ino : Int = throw new InvalidSelector("ino undefined")
  def updated_ino(__x : Int) : key = throw new InvalidSelectorUpdate("updated_ino undefined")
  def part : Int = throw new InvalidSelector("part undefined")
  def updated_part(__x : Int) : key = throw new InvalidSelectorUpdate("updated_part undefined")
  def name : String = throw new InvalidSelector("name undefined")
  def updated_name(__x : String) : key = throw new InvalidSelectorUpdate("updated_name undefined")
}

object key {
  /**
   * case-classes and objects for constructors
   */
  final case class inodekey(override val ino : Int) extends key {
    override def updated_ino(__x : Int) : inodekey = copy(ino = __x)
  }
  final case class datakey(override val ino : Int, override val part : Int) extends key {
    override def updated_ino(__x : Int) : datakey = copy(ino = __x)
    override def updated_part(__x : Int) : datakey = copy(part = __x)
  }
  final case class dentrykey(override val ino : Int, override val name : String) extends key {
    override def updated_ino(__x : Int) : dentrykey = copy(ino = __x)
    override def updated_name(__x : String) : dentrykey = copy(name = __x)
  }

  def uninit = inodekey(0)

  implicit object Randomizer extends helpers.scala.Randomizer[key] {
    override def random(): key = helpers.scala.Random.generator.nextInt(3) match {
      case 0 => inodekey(helpers.scala.Random[Int])
      case 1 => datakey(helpers.scala.Random[Int], helpers.scala.Random[Int])
      case 2 => dentrykey(helpers.scala.Random[Int], helpers.scala.Random[String])
    }
  }
}
