// Flashix: a verified file system for flash memory
// (c) 2015-2019 Institute for Software & Systems Engineering <http://isse.de/flashix>
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
  def get_max_pageno(INO: Int, PAGENO: Ref[Int])
  def recovery(ERR: Ref[error])
  def set(INO: Int, PAGENO: Int, BUF: buffer, DIRTY: Boolean, ERR: Ref[error])
  def set_status(INO: Int, PAGENO: Int, DIRTY: Boolean, ERR: Ref[error])
  def set_status_nofail(INO: Int, PAGENO: Int, DIRTY: Boolean)
  def truncate(INO: Int, SIZE: Int, N: Int, ERR: Ref[error])
  def write_begin(INO: Int, SIZE: Int)
}

class Pcache(val PCACHE : pcache)(implicit _algebraic_implicit: algebraic.Algebraic) extends PcacheInterface {
  import _algebraic_implicit._

  def delete(INO: Int, PAGENO: Int, ERR: Ref[error]): Unit = {
    PCACHE -= types.key.datakey(INO, PAGENO)
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
    HIT := PCACHE.contains(KEY)
    if (HIT.get) {
      val PE: pcache_entry = PCACHE(KEY).deepCopy
      BUF := PE.page.deepCopy
      DIRTY := PE.dirty
    }
    ERR := types.error.ESUCCESS
  }

  def get_max_pageno(INO: Int, PAGENO: Ref[Int]): Unit = {
    PAGENO := max(keys(PCACHE, INO))
  }

  def recovery(ERR: Ref[error]): Unit = {
    PCACHE.clear
    ERR := types.error.ESUCCESS
  }

  def set(INO: Int, PAGENO: Int, BUF: buffer, DIRTY: Boolean, ERR: Ref[error]): Unit = {
    PCACHE(types.key.datakey(INO, PAGENO)) = types.pcache_entry.P(DIRTY, BUF).deepCopy
    ERR := types.error.ESUCCESS
  }

  def set_status(INO: Int, PAGENO: Int, DIRTY: Boolean, ERR: Ref[error]): Unit = {
    val KEY: key = types.key.datakey(INO, PAGENO)
    if (PCACHE.contains(KEY)) {
      PCACHE(KEY).dirty = DIRTY
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

  def write_begin(INO: Int, SIZE: Int): Unit = {
    PCACHE := _algebraic_implicit.truncate(INO, SIZE, PCACHE).deepCopy
  }

}
