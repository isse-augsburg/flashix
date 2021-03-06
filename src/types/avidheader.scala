// Flashix: a verified file system for flash memory
// (c) 2015-2019 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._

sealed abstract class avidheader {
  def vol : Byte = throw new InvalidSelector("vol undefined")
  def updated_vol(__x : Byte) : avidheader = throw new InvalidSelectorUpdate("updated_vol undefined")
  def leb : Int = throw new InvalidSelector("leb undefined")
  def updated_leb(__x : Int) : avidheader = throw new InvalidSelectorUpdate("updated_leb undefined")
  def sqn : Int = throw new InvalidSelector("sqn undefined")
  def updated_sqn(__x : Int) : avidheader = throw new InvalidSelectorUpdate("updated_sqn undefined")
  def size : Int = throw new InvalidSelector("size undefined")
  def updated_size(__x : Int) : avidheader = throw new InvalidSelectorUpdate("updated_size undefined")
  def checksum : Int = throw new InvalidSelector("checksum undefined")
  def updated_checksum(__x : Int) : avidheader = throw new InvalidSelectorUpdate("updated_checksum undefined")
}

object avidheader {
  /**
   * case-classes and objects for constructors
   */
  final case class avidhdr(override val vol : Byte, override val leb : Int, override val sqn : Int, override val size : Int, override val checksum : Int) extends avidheader {
    override def updated_vol(__x : Byte) : avidhdr = copy(vol = __x)
    override def updated_leb(__x : Int) : avidhdr = copy(leb = __x)
    override def updated_sqn(__x : Int) : avidhdr = copy(sqn = __x)
    override def updated_size(__x : Int) : avidhdr = copy(size = __x)
    override def updated_checksum(__x : Int) : avidhdr = copy(checksum = __x)
  }
  final object empty extends avidheader
  final object garbage extends avidheader

  def uninit = empty

  implicit object Randomizer extends helpers.scala.Randomizer[avidheader] {
    override def random(): avidheader = helpers.scala.Random.generator.nextInt(3) match {
      case 0 => empty
      case 1 => garbage
      case 2 => avidhdr(helpers.scala.Random[Byte], helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[Int])
    }
  }
}
