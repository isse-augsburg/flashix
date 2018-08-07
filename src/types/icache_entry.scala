// Flashix: a verified file system for flash memory
// (c) 2015-2018 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._

final case class icache_entry(dirty: Boolean, inode: inode) extends DeepCopyable[icache_entry] {
  override def deepCopy(): icache_entry = icache_entry(dirty, inode.deepCopy)
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
