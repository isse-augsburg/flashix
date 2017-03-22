// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

sealed abstract class ebaentry {
  def pnum : Int = throw new InvalidSelector("pnum undefined")
  def updated_pnum(__x : Int) : ebaentry = throw new InvalidSelectorUpdate("updated_pnum undefined")
}

object ebaentry {
  /**
   * case-classes and objects for constructors
   */
  final case class embed(override val pnum : Int) extends ebaentry {
    override def updated_pnum(__x : Int) : embed = copy(pnum = __x)
  }
  final object unmapped extends ebaentry

  def uninit = unmapped

  implicit object Randomizer extends helpers.scala.Randomizer[ebaentry] {
    override def random(): ebaentry = helpers.scala.Random.generator.nextInt(2) match {
      case 0 => unmapped
      case 1 => embed(helpers.scala.Random[Int])
    }
  }
}
