// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

final case class icache_entry(var dirty: Boolean, var inode: inode) extends DeepCopyable[icache_entry] {
  override def deepCopy(): icache_entry = icache_entry(dirty, inode.deepCopy)

  def := (other: icache_entry) {
    dirty = other.dirty
    inode = other.inode
  }
}

object icache_entry {
  /**
   * Functions for constructors
   */
  def mkientry(dirty: Boolean, inode: inode): icache_entry = {
    icache_entry(dirty, inode)
  }

  def uninit = mkientry(helpers.scala.Boolean.uninit, types.inode.uninit)

  implicit object Randomizer extends helpers.scala.Randomizer[icache_entry] {
    override def random(): icache_entry = icache_entry(helpers.scala.Random[Boolean], helpers.scala.Random[inode])
  }
}
