// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

final case class inode(var ino: Int, var meta: metadata, var directory: Boolean, var nlink: Int, var nsubdirs: Int, var size: Int) extends DeepCopyable[inode] {
  override def deepCopy(): inode = inode(ino, meta, directory, nlink, nsubdirs, size)

  def := (other: inode) {
    ino = other.ino
    meta = other.meta
    directory = other.directory
    nlink = other.nlink
    nsubdirs = other.nsubdirs
    size = other.size
  }
}

object inode {
  /**
   * Functions for constructors
   */
  def mkinode(ino: Int, meta: metadata, directory: Boolean, nlink: Int, nsubdirs: Int, size: Int): inode = {
    inode(ino, meta, directory, nlink, nsubdirs, size)
  }

  def uninit = mkinode(0, types.metadata.uninit, helpers.scala.Boolean.uninit, 0, 0, 0)

  implicit object Randomizer extends helpers.scala.Randomizer[inode] {
    override def random(): inode = inode(helpers.scala.Random[Int], helpers.scala.Random[metadata], helpers.scala.Random[Boolean], helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[Int])
  }
}
