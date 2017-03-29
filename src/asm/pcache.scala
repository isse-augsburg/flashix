// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error

abstract class pcache_interface {
  def check_page(INO: Int, PAGENO: Int, HIT: Ref[Boolean], DIRTY: Ref[Boolean], ERR: Ref[error])
  def delete(INO: Int, PAGENO: Int, ERR: Ref[error])
  def evict(INO: Int, ERR: Ref[error])
  def format(ERR: Ref[error])
  def get(INO: Int, PAGENO: Int, HIT: Ref[Boolean], BUF: buffer, ERR: Ref[error])
  def recovery(ERR: Ref[error])
  def set(INO: Int, PAGENO: Int, BUF: buffer, DIRTY: Boolean, ERR: Ref[error])
  def set_status(INO: Int, PAGENO: Int, DIRTY: Boolean, ERR: Ref[error])
  def truncate(INO: Int, SIZE: Int, N: Int, ERR: Ref[error])
}
class pcache_asm(val PCACHE : pcache)(implicit _algebraic_implicit: algebraic.Algebraic) extends pcache_interface {
  import _algebraic_implicit._

  def check_page(INO: Int, PAGENO: Int, HIT: Ref[Boolean], DIRTY: Ref[Boolean], ERR: Ref[error]): Unit = {
    val KEY: key = types.key.datakey(INO, PAGENO)
    if (PCACHE.contains(KEY)) {
      HIT := true
      DIRTY := PCACHE(KEY).dirty
    } else {
      HIT := false
    }
  }

  def delete(INO: Int, PAGENO: Int, ERR: Ref[error]): Unit = {
    val KEY: key = types.key.datakey(INO, PAGENO)
    PCACHE -= KEY
  }

  def evict(INO: Int, ERR: Ref[error]): Unit = {
    PCACHE := _algebraic_implicit.evict(INO, PCACHE).deepCopy
  }

  def format(ERR: Ref[error]): Unit = {
    PCACHE.clear
    ERR := types.error.ESUCCESS
  }

  def get(INO: Int, PAGENO: Int, HIT: Ref[Boolean], BUF: buffer, ERR: Ref[error]): Unit = {
    val KEY: key = types.key.datakey(INO, PAGENO)
    if (PCACHE.contains(KEY)) {
      val PE: pcache_entry = PCACHE(KEY).deepCopy
      BUF := PE.page.deepCopy
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
  }

  def set_status(INO: Int, PAGENO: Int, DIRTY: Boolean, ERR: Ref[error]): Unit = {
    val KEY: key = types.key.datakey(INO, PAGENO)
    if (PCACHE.contains(KEY)) {
      var PE: pcache_entry = PCACHE(KEY).deepCopy
      PE = types.pcache_entry.mkpentry(DIRTY, PE.page).deepCopy
      PCACHE(KEY) = PE
    } else {
      ERR := types.error.EFAIL
    }
  }

  def truncate(INO: Int, SIZE: Int, N: Int, ERR: Ref[error]): Unit = {
    PCACHE := _algebraic_implicit.truncate(INO, min(N, SIZE), PCACHE).deepCopy
  }

}

