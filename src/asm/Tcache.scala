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

abstract class TcacheInterface extends ASM {
  def delete(INO: Int)
  def format(ERR: Ref[error])
  def get(INO: Int, TRUNC_SIZE: Ref[Int], HIT: Ref[Boolean])
  def recovery(ERR: Ref[error])
  def update(INO: Int, TRUNC_SIZE: Int, ERR: Ref[error])
  def update_old(INO: Int, SIZE: Int, TRUNC_SIZE: Int, ERR: Ref[error])
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

  def get(INO: Int, TRUNC_SIZE: Ref[Int], HIT: Ref[Boolean]): Unit = {
    HIT := TCACHE.contains(INO)
    if (HIT.get) {
      TRUNC_SIZE := TCACHE(INO)
    }
  }

  def recovery(ERR: Ref[error]): Unit = {
    TCACHE.clear
    ERR := types.error.ESUCCESS
  }

  def update(INO: Int, TRUNC_SIZE: Int, ERR: Ref[error]): Unit = {
    if (! TCACHE.contains(INO) || TCACHE.contains(INO) && TRUNC_SIZE < TCACHE(INO)) {
      TCACHE(INO) = TRUNC_SIZE
    }
    ERR := types.error.ESUCCESS
  }

  def update_old(INO: Int, SIZE: Int, TRUNC_SIZE: Int, ERR: Ref[error]): Unit = {
    val NEW_TRUNC_SIZE: Int = min(SIZE, TRUNC_SIZE)
    if (! TCACHE.contains(INO) || TCACHE.contains(INO) && NEW_TRUNC_SIZE < TCACHE(INO)) {
      TCACHE(INO) = NEW_TRUNC_SIZE
    }
    ERR := types.error.ESUCCESS
  }

}
