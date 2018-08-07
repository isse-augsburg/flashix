// Flashix: a verified file system for flash memory
// (c) 2015-2018 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._
import types._
import types.error.error

abstract class IcacheInterface extends ASM {
  def delete(INO: Int, ERR: Ref[error])
  def delete_old(INO: Int)
  def exchange_old(INODE: inode, ERR: Ref[error])
  def format(ERR: Ref[error])
  def get(INO: Int, INODE: inode, DIRTY: Ref[Boolean], HIT: Ref[Boolean], ERR: Ref[error])
  def get_old(INO: Int, MD: Ref[metadata], SIZE: Ref[Int], HIT: Ref[Boolean])
  def get_status(INO: Int, DIRTY: Ref[Boolean], HIT: Ref[Boolean])
  def recovery(ERR: Ref[error])
  def set(INODE: inode, DIRTY: Boolean, ERR: Ref[error])
  def set_inode(INODE: inode, ERR: Ref[error])
  def set_old(INODE: inode, ERR: Ref[error])
  def set_status(INO: Int, DIRTY: Boolean, ERR: Ref[error])
}

class Icache(val ICACHE : icache, val MSCACHE : mscache)(implicit _algebraic_implicit: algebraic.Algebraic) extends IcacheInterface {
  import _algebraic_implicit._

  def delete(INO: Int, ERR: Ref[error]): Unit = {
    ICACHE -= INO
    ERR := types.error.ESUCCESS
  }

  def delete_old(INO: Int): Unit = {
    MSCACHE -= INO
  }

  def exchange_old(INODE: inode, ERR: Ref[error]): Unit = {
    val INO: Int = INODE.ino
    val ISDIR: Boolean = INODE.directory
    val EXISTS: Boolean = MSCACHE.contains(INODE.ino)
    INODE.meta = (if (EXISTS) MSCACHE(INO).meta else INODE.meta)
    INODE.size = (if (EXISTS && ISDIR != true) MSCACHE(INO).size else INODE.size)
    ERR := types.error.ESUCCESS
  }

  def format(ERR: Ref[error]): Unit = {
    ICACHE.clear
    MSCACHE.clear
    ERR := types.error.ESUCCESS
  }

  def get(INO: Int, INODE: inode, DIRTY: Ref[Boolean], HIT: Ref[Boolean], ERR: Ref[error]): Unit = {
    if (ICACHE.contains(INO)) {
      INODE := ICACHE(INO).inode.deepCopy
      DIRTY := ICACHE(INO).dirty
      HIT := true
    } else {
      HIT := false
    }
    ERR := types.error.ESUCCESS
  }

  def get_old(INO: Int, MD: Ref[metadata], SIZE: Ref[Int], HIT: Ref[Boolean]): Unit = {
    HIT := MSCACHE.contains(INO)
    if (HIT.get) {
      MD := MSCACHE(INO).meta
      SIZE := MSCACHE(INO).size
    }
  }

  def get_status(INO: Int, DIRTY: Ref[Boolean], HIT: Ref[Boolean]): Unit = {
    HIT := ICACHE.contains(INO)
    if (HIT.get) {
      DIRTY := ICACHE(INO).dirty
    }
  }

  def recovery(ERR: Ref[error]): Unit = {
    ICACHE.clear
    MSCACHE.clear
    ERR := types.error.ESUCCESS
  }

  def set(INODE: inode, DIRTY: Boolean, ERR: Ref[error]): Unit = {
    ICACHE(INODE.ino) = types.icache_entry.mkientry(DIRTY, INODE).deepCopy
    ERR := types.error.ESUCCESS
  }

  def set_inode(INODE: inode, ERR: Ref[error]): Unit = {
    val INO: Int = INODE.ino
    val DIRTY: Boolean = if (ICACHE.contains(INO)) ICACHE(INO).dirty else false
    val IE: icache_entry = types.icache_entry.mkientry(DIRTY, INODE).deepCopy
    ICACHE(INO) = IE
    ERR := types.error.ESUCCESS
  }

  def set_old(INODE: inode, ERR: Ref[error]): Unit = {
    val SIZE: Int = if (INODE.directory) 0 else INODE.size
    MSCACHE(INODE.ino) = types.meta_size.metasize(INODE.meta, SIZE)
    ERR := types.error.ESUCCESS
  }

  def set_status(INO: Int, DIRTY: Boolean, ERR: Ref[error]): Unit = {
    if (ICACHE.contains(INO)) {
      var IE: icache_entry = ICACHE(INO).deepCopy
      IE = types.icache_entry.mkientry(DIRTY, IE.inode).deepCopy
      ICACHE(INO) = IE
      ERR := types.error.ESUCCESS
    } else {
      ERR := types.error.EFAIL
    }
  }

}
