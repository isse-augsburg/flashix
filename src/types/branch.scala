// Flashix: a verified file system for flash memory
// (c) 2015-2019 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._

sealed abstract class branch {
  def key : key = throw new InvalidSelector("key undefined")
  def updated_key(__x : key) : branch = throw new InvalidSelectorUpdate("updated_key undefined")
  def adr : address = throw new InvalidSelector("adr undefined")
  def updated_adr(__x : address) : branch = throw new InvalidSelectorUpdate("updated_adr undefined")
}

object branch {
  /**
   * case-classes and objects for constructors
   */
  final case class mkbranch(override val key : key, override val adr : address) extends branch {
    override def updated_key(__x : key) : mkbranch = copy(key = __x)
    override def updated_adr(__x : address) : mkbranch = copy(adr = __x)
  }
  final case class mkentry(override val key : key, override val adr : address) extends branch {
    override def updated_key(__x : key) : mkentry = copy(key = __x)
    override def updated_adr(__x : address) : mkentry = copy(adr = __x)
  }

  def uninit = mkbranch(types.key.uninit, types.address.uninit)

  implicit object Randomizer extends helpers.scala.Randomizer[branch] {
    override def random(): branch = helpers.scala.Random.generator.nextInt(2) match {
      case 0 => mkbranch(helpers.scala.Random[key], helpers.scala.Random[address])
      case 1 => mkentry(helpers.scala.Random[key], helpers.scala.Random[address])
    }
  }
}
