// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error

abstract class dcache_interface {
  def delete(P_INO: Int, NAME: String, ERR: Ref[error])
  def format(ERR: Ref[error])
  def get(P_INO: Int, NAME: String, HIT: Ref[Boolean], DENT: Ref[dentry], ERR: Ref[error])
  def recovery(ERR: Ref[error])
  def set(P_INO: Int, NAME: String, DENT: dentry, ERR: Ref[error])
}
class dcache_asm(val DCACHE : dcache)(implicit _algebraic_implicit: algebraic.Algebraic) extends dcache_interface {
  import _algebraic_implicit._

  def delete(P_INO: Int, NAME: String, ERR: Ref[error]): Unit = {
    val KEY: key = types.key.dentrykey(P_INO, NAME)
    DCACHE -= KEY
  }

  def format(ERR: Ref[error]): Unit = {
    DCACHE.clear
    ERR := types.error.ESUCCESS
  }

  def get(P_INO: Int, NAME: String, HIT: Ref[Boolean], DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    val KEY: key = types.key.dentrykey(P_INO, NAME)
    if (DCACHE.contains(KEY)) {
      DENT := DCACHE(KEY)
      HIT := true
    } else {
      HIT := false
    }
    ERR := types.error.ESUCCESS
  }

  def recovery(ERR: Ref[error]): Unit = {
    DCACHE.clear
    ERR := types.error.ESUCCESS
  }

  def set(P_INO: Int, NAME: String, DENT: dentry, ERR: Ref[error]): Unit = {
    val KEY: key = types.key.dentrykey(P_INO, NAME)
    DCACHE(KEY) = DENT
  }

}

