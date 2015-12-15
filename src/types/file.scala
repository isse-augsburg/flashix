// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import types.file_mode.file_mode

final case class file(var ino: Int, var mode: file_mode, var pos: Int) extends DeepCopyable[file] {
  override def deepCopy(): file = file(ino, mode, pos)
}

object file {
  /**
   * Functions for constructors
   */
  def mkfile(ino: Int, mode: file_mode, pos: Int) : file = {
    file(ino, mode, pos)
  }

  def uninit = mkfile(0, file_mode.uninit, 0)

  implicit object Randomizer extends helpers.scala.Randomizer[file] {
    def random() : file = file(helpers.scala.Random[Int], helpers.scala.Random[file_mode], helpers.scala.Random[Int])
  }
}
