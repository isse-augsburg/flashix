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

abstract class DcacheInterface extends ASM {
  def delete(P_INO: Int, NAME: String, ERR: Ref[error])
  def format(ERR: Ref[error])
  def get(P_INO: Int, NAME: String, DENT: Ref[dentry], HIT: Ref[Boolean], ERR: Ref[error])
  def recovery(ERR: Ref[error])
  def set(P_INO: Int, NAME: String, DENT: dentry, ERR: Ref[error])
}

class Dcache(val DCACHE : dcache)(implicit _algebraic_implicit: algebraic.Algebraic) extends DcacheInterface {
  import _algebraic_implicit._

  def delete(P_INO: Int, NAME: String, ERR: Ref[error]): Unit = {
    val KEY: key = types.key.dentrykey(P_INO, NAME)
    DCACHE -= KEY
    ERR := types.error.ESUCCESS
  }

  def format(ERR: Ref[error]): Unit = {
    DCACHE.clear
    ERR := types.error.ESUCCESS
  }

  def get(P_INO: Int, NAME: String, DENT: Ref[dentry], HIT: Ref[Boolean], ERR: Ref[error]): Unit = {
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
    ERR := types.error.ESUCCESS
  }

}
