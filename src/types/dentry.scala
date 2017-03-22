// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

sealed abstract class dentry {
  def name : String = throw new InvalidSelector("name undefined")
  def updated_name(__x : String) : dentry = throw new InvalidSelectorUpdate("updated_name undefined")
  def ino : Int = throw new InvalidSelector("ino undefined")
  def updated_ino(__x : Int) : dentry = throw new InvalidSelectorUpdate("updated_ino undefined")
}

object dentry {
  /**
   * case-classes and objects for constructors
   */
  final case class mkdentry(override val name : String, override val ino : Int) extends dentry {
    override def updated_name(__x : String) : mkdentry = copy(name = __x)
    override def updated_ino(__x : Int) : mkdentry = copy(ino = __x)
  }
  final case class negdentry(override val name : String) extends dentry {
    override def updated_name(__x : String) : negdentry = copy(name = __x)
  }

  def uninit = mkdentry("", 0)

  implicit object Randomizer extends helpers.scala.Randomizer[dentry] {
    override def random(): dentry = helpers.scala.Random.generator.nextInt(2) match {
      case 0 => mkdentry(helpers.scala.Random[String], helpers.scala.Random[Int])
      case 1 => negdentry(helpers.scala.Random[String])
    }
  }
}
