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

abstract class PcacheInterface extends ASM {
  def delete(INO: Int, PAGENO: Int, ERR: Ref[error])
  def evict(INO: Int, ERR: Ref[error])
  def format(ERR: Ref[error])
  def get(INO: Int, PAGENO: Int, BUF: buffer, DIRTY: Ref[Boolean], HIT: Ref[Boolean], ERR: Ref[error])
  def recovery(ERR: Ref[error])
  def set(INO: Int, PAGENO: Int, BUF: buffer, DIRTY: Boolean, ERR: Ref[error])
  def set_status(INO: Int, PAGENO: Int, DIRTY: Boolean, ERR: Ref[error])
  def set_status_nofail(INO: Int, PAGENO: Int, DIRTY: Boolean)
  def truncate(INO: Int, SIZE: Int, N: Int, ERR: Ref[error])
}

class Pcache(val PCACHE : pcache)(implicit _algebraic_implicit: algebraic.Algebraic) extends PcacheInterface {
  import _algebraic_implicit._

  def delete(INO: Int, PAGENO: Int, ERR: Ref[error]): Unit = {
    val KEY: key = types.key.datakey(INO, PAGENO)
    PCACHE -= KEY
    ERR := types.error.ESUCCESS
  }

  def evict(INO: Int, ERR: Ref[error]): Unit = {
    PCACHE := _algebraic_implicit.evict(INO, PCACHE).deepCopy
    ERR := types.error.ESUCCESS
  }

  def format(ERR: Ref[error]): Unit = {
    PCACHE.clear
    ERR := types.error.ESUCCESS
  }

  def get(INO: Int, PAGENO: Int, BUF: buffer, DIRTY: Ref[Boolean], HIT: Ref[Boolean], ERR: Ref[error]): Unit = {
    val KEY: key = types.key.datakey(INO, PAGENO)
    if (PCACHE.contains(KEY)) {
      val PE: pcache_entry = PCACHE(KEY).deepCopy
      BUF := PE.page.deepCopy
      DIRTY := PE.dirty
      HIT := true
    } else {
      HIT := false
    }
    ERR := types.error.ESUCCESS
  }

  def recovery(ERR: Ref[error]): Unit = {
    PCACHE.clear
    ERR := types.error.ESUCCESS
  }

  def set(INO: Int, PAGENO: Int, BUF: buffer, DIRTY: Boolean, ERR: Ref[error]): Unit = {
    val KEY: key = types.key.datakey(INO, PAGENO)
    val PE: pcache_entry = types.pcache_entry.mkpentry(DIRTY, BUF).deepCopy
    PCACHE(KEY) = PE
    ERR := types.error.ESUCCESS
  }

  def set_status(INO: Int, PAGENO: Int, DIRTY: Boolean, ERR: Ref[error]): Unit = {
    val KEY: key = types.key.datakey(INO, PAGENO)
    if (PCACHE.contains(KEY)) {
      val PE: pcache_entry = PCACHE(KEY).deepCopy
      PE := types.pcache_entry.mkpentry(DIRTY, PE.page).deepCopy
      PCACHE(KEY) = PE
      ERR := types.error.ESUCCESS
    } else {
      ERR := types.error.EFAIL
    }
  }

  def set_status_nofail(INO: Int, PAGENO: Int, DIRTY: Boolean): Unit = {
    val KEY: key = types.key.datakey(INO, PAGENO)
    if (PCACHE.contains(KEY)) {
      PCACHE(KEY).dirty = DIRTY
    }
  }

  def truncate(INO: Int, SIZE: Int, N: Int, ERR: Ref[error]): Unit = {
    PCACHE := _algebraic_implicit.truncate(INO, min(N, SIZE), PCACHE).deepCopy
    ERR := types.error.ESUCCESS
  }

}
