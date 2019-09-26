// Flashix: a verified file system for flash memory
// (c) 2015-2019 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._

sealed abstract class zbranch {
  def key : key = throw new InvalidSelector("key undefined")
  def updated_key(__x : key) : zbranch = throw new InvalidSelectorUpdate("updated_key undefined")
  def adr : address = throw new InvalidSelector("adr undefined")
  def updated_adr(__x : address) : zbranch = throw new InvalidSelectorUpdate("updated_adr undefined")
  def child : znode = throw new InvalidSelector("child undefined")
  def updated_child(__x : znode) : zbranch = throw new InvalidSelectorUpdate("updated_child undefined")
}

object zbranch {
  /**
   * case-classes and objects for constructors
   */
  final case class mkzbranch(override val key : key, override val adr : address, override val child : znode) extends zbranch {
    override def updated_key(__x : key) : mkzbranch = copy(key = __x)
    override def updated_adr(__x : address) : mkzbranch = copy(adr = __x)
    override def updated_child(__x : znode) : mkzbranch = copy(child = __x)
  }
  final case class mkzentry(override val key : key, override val adr : address) extends zbranch {
    override def updated_key(__x : key) : mkzentry = copy(key = __x)
    override def updated_adr(__x : address) : mkzentry = copy(adr = __x)
  }

  def uninit = mkzbranch(types.key.uninit, types.address.uninit, null)

  implicit object Randomizer extends helpers.scala.Randomizer[zbranch] {
    override def random(): zbranch = helpers.scala.Random.generator.nextInt(2) match {
      case 0 => mkzbranch(helpers.scala.Random[key], helpers.scala.Random[address], helpers.scala.Random[znode])
      case 1 => mkzentry(helpers.scala.Random[key], helpers.scala.Random[address])
    }
  }
}
