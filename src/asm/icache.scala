// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error

abstract class icache_interface {
  def check_inode(INO: Int, HIT: Ref[Boolean], DIRTY: Ref[Boolean], ERR: Ref[error])
  def delete(INO: Int, ERR: Ref[error])
  def format(ERR: Ref[error])
  def get(INO: Int, HIT: Ref[Boolean], INODE: inode, ERR: Ref[error])
  def recovery(ERR: Ref[error])
  def set(INODE: inode, DIRTY: Boolean, ERR: Ref[error])
  def set_status(INO: Int, DIRTY: Boolean, ERR: Ref[error])
}
class icache_asm(val ICACHE : icache)(implicit _algebraic_implicit: algebraic.Algebraic) extends icache_interface {
  import _algebraic_implicit._

  def check_inode(INO: Int, HIT: Ref[Boolean], DIRTY: Ref[Boolean], ERR: Ref[error]): Unit = {
    if (ICACHE.contains(INO)) {
      HIT := true
      DIRTY := ICACHE(INO).dirty
    } else {
      HIT := false
    }
  }

  def delete(INO: Int, ERR: Ref[error]): Unit = {
    ICACHE -= INO
  }

  def format(ERR: Ref[error]): Unit = {
    ICACHE.clear
    ERR := types.error.ESUCCESS
  }

  def get(INO: Int, HIT: Ref[Boolean], INODE: inode, ERR: Ref[error]): Unit = {
    if (ICACHE.contains(INO)) {
      INODE := ICACHE(INO).inode.deepCopy
      HIT := true
    } else {
      HIT := false
    }
    ERR := types.error.ESUCCESS
  }

  def recovery(ERR: Ref[error]): Unit = {
    ICACHE.clear
    ERR := types.error.ESUCCESS
  }

  def set(INODE: inode, DIRTY: Boolean, ERR: Ref[error]): Unit = {
    ICACHE(INODE.ino) = types.icache_entry.mkientry(DIRTY, INODE).deepCopy
  }

  def set_status(INO: Int, DIRTY: Boolean, ERR: Ref[error]): Unit = {
    if (ICACHE.contains(INO)) {
      ICACHE(INO).dirty = DIRTY
    } else {
      ERR := types.error.EFAIL
    }
  }

}

