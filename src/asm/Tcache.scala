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

abstract class TcacheInterface extends ASM {
  def delete(INO: Int)
  def format(ERR: Ref[error])
  def get(INO: Int, MIN_TRUNC_SIZE: Ref[Int], LAST_TRUNC_SIZE: Ref[Int], MINUP_EXISTS: Ref[Boolean], HIT: Ref[Boolean])
  def recovery(ERR: Ref[error])
  def update(INO: Int, TRUNC_SIZE: Int, FSIZE: Int, ERR: Ref[error])
  def write_begin(INO: Int, SIZE: Int)
}

class Tcache(val TCACHE : tcache)(implicit _algebraic_implicit: algebraic.Algebraic) extends TcacheInterface {
  import _algebraic_implicit._

  def delete(INO: Int): Unit = {
    TCACHE -= INO
  }

  def format(ERR: Ref[error]): Unit = {
    TCACHE.clear
    ERR := types.error.ESUCCESS
  }

  def get(INO: Int, MIN_TRUNC_SIZE: Ref[Int], LAST_TRUNC_SIZE: Ref[Int], MINUP_EXISTS: Ref[Boolean], HIT: Ref[Boolean]): Unit = {
    HIT := TCACHE.contains(INO)
    if (HIT.get) {
      LAST_TRUNC_SIZE := last(TCACHE(INO))
      MINUP_EXISTS := TCACHE(INO).isInstanceOf[types.tcache_entry.T]
      MIN_TRUNC_SIZE := min(TCACHE(INO))
    }
  }

  def recovery(ERR: Ref[error]): Unit = {
    TCACHE.clear
    ERR := types.error.ESUCCESS
  }

  def update(INO: Int, TRUNC_SIZE: Int, FSIZE: Int, ERR: Ref[error]): Unit = {
    TCACHE := _algebraic_implicit.update(TCACHE, INO, TRUNC_SIZE, FSIZE).deepCopy
    ERR := types.error.ESUCCESS
  }

  def write_begin(INO: Int, SIZE: Int): Unit = {
    TCACHE := _algebraic_implicit.update(TCACHE, INO, SIZE, SIZE).deepCopy
  }

}
