// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package encoding

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import sorts._
import types._
import types.error.error
import types.file_mode.file_mode
import types.lpropflags.lpropflags
import types.seekflag.seekflag
import types.wlstatus.wlstatus

object data {
  def ENCODED_DATA_SIZE(implicit _algebraic_implicit: algebraic.Algebraic): Int = {
    return 1
  }

  def encode_data(elem: Byte, index: Int, buf: buffer, nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    err := types.error.ESUCCESS
    nbytes := 1
    buf(index) = elem
  }

  def decode_data(index: Int, buf: buffer, elem: Ref[Byte], nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    err := types.error.ESUCCESS
    nbytes := 1
    elem := buf(index)
  }
}
