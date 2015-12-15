// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._

sealed abstract class ebaentry {
  def pnum : Int = throw new InvalidSelector("pnum undefined")
  def updated_pnum(__x : Int) : ebaentry = throw new InvalidSelectorUpdate("updated_pnum undefined")
}

object ebaentry {
  implicit object Randomizer extends helpers.scala.Randomizer[ebaentry] {
    def random() : ebaentry = unmapped
  }

  /**
   * case-classes and objects for constructors
   */
  final case class embed(override val pnum : Int) extends ebaentry {
    override def updated_pnum(__x : Int) : embed = copy(pnum = __x)
  }
  final object unmapped extends ebaentry

  def uninit = unmapped
}
