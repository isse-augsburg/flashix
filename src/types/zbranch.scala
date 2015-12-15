// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._

sealed abstract class zbranch extends DeepCopyable[zbranch] {
  def key : key = throw new InvalidSelector("key undefined")
  def updated_key(__x : key) : zbranch = throw new InvalidSelectorUpdate("updated_key undefined")
  def adr : address = throw new InvalidSelector("adr undefined")
  def updated_adr(__x : address) : zbranch = throw new InvalidSelectorUpdate("updated_adr undefined")
  def child : znode = throw new InvalidSelector("child undefined")
  def updated_child(__x : znode) : zbranch = throw new InvalidSelectorUpdate("updated_child undefined")
  def nd : node_option = throw new InvalidSelector("nd undefined")
  def updated_nd(__x : node_option) : zbranch = throw new InvalidSelectorUpdate("updated_nd undefined")
}

object zbranch {
  implicit object Randomizer extends helpers.scala.Randomizer[zbranch] {
    def random() : zbranch = mkzbranch(helpers.scala.Random[key], helpers.scala.Random[address], helpers.scala.Random[znode])
  }

  /**
   * case-classes and objects for constructors
   */
  final case class mkzbranch(override val key : key, override val adr : address, override val child : znode) extends zbranch {
    override def updated_key(__x : key) : mkzbranch = copy(key = __x)
    override def updated_adr(__x : address) : mkzbranch = copy(adr = __x)
    override def updated_child(__x : znode) : mkzbranch = copy(child = __x)
    override def deepCopy(): zbranch = mkzbranch(key, adr, child)
  }
  final case class mkzentry(override val key : key, override val adr : address, override val nd : node_option) extends zbranch {
    override def updated_key(__x : key) : mkzentry = copy(key = __x)
    override def updated_adr(__x : address) : mkzentry = copy(adr = __x)
    override def updated_nd(__x : node_option) : mkzentry = copy(nd = __x)
    override def deepCopy(): zbranch = mkzentry(key, adr, nd.deepCopy)
  }
  final case class mkzchecked(override val key : key) extends zbranch {
    override def updated_key(__x : key) : mkzchecked = copy(key = __x)
    override def deepCopy(): zbranch = mkzchecked(key)
  }

  def uninit = mkzbranch(key.uninit, address.uninit, null)
}
