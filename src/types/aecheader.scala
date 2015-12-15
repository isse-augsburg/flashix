// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._

sealed abstract class aecheader {
  def ec : Int = throw new InvalidSelector("ec undefined")
  def updated_ec(__x : Int) : aecheader = throw new InvalidSelectorUpdate("updated_ec undefined")
}

object aecheader {
  implicit object Randomizer extends helpers.scala.Randomizer[aecheader] {
    def random() : aecheader = empty
  }

  /**
   * case-classes and objects for constructors
   */
  final case class aechdr(override val ec : Int) extends aecheader {
    override def updated_ec(__x : Int) : aechdr = copy(ec = __x)
  }
  final object empty extends aecheader
  final object garbage extends aecheader

  def uninit = empty
}
