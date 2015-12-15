// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._

sealed abstract class lebref {
  def vol : Byte = throw new InvalidSelector("vol undefined")
  def updated_vol(__x : Byte) : lebref = throw new InvalidSelectorUpdate("updated_vol undefined")
  def leb : Int = throw new InvalidSelector("leb undefined")
  def updated_leb(__x : Int) : lebref = throw new InvalidSelectorUpdate("updated_leb undefined")
}

object lebref {
  implicit object Randomizer extends helpers.scala.Randomizer[lebref] {
    def random() : lebref = none
  }

  /**
   * case-classes and objects for constructors
   */
  final case class ×(override val vol : Byte, override val leb : Int) extends lebref {
    override def updated_vol(__x : Byte) : × = copy(vol = __x)
    override def updated_leb(__x : Int) : × = copy(leb = __x)
  }
  final object none extends lebref

  def uninit = none
}
