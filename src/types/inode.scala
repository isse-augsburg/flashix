package types

import helpers.scala._

final case class inode(ino: Int, meta: metadata, directory: Boolean, nlink: Int, size: Int) {}

object inode {
  /**
   * Functions for constructors
   */
  def mkinode(ino: Int, meta: metadata, directory: Boolean, nlink: Int, size: Int) : inode = {
    inode(ino, meta, directory, nlink, size)
  }

  def uninit = mkinode(0, metadata.uninit, helpers.scala.Boolean.uninit, 0, 0)

  implicit object Randomizer extends helpers.scala.Randomizer[inode] {
    def random() : inode = inode(helpers.scala.Random[Int], helpers.scala.Random[metadata], helpers.scala.Random[Boolean], helpers.scala.Random[Int], helpers.scala.Random[Int])
  }
}
