// Flashix: a verified file system for flash memory
// (c) 2015-2019 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package encoding

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._
import types._
import types.error.error
import types.file_mode.file_mode
import types.lpropflags.lpropflags
import types.seekflag.seekflag
import types.wlstatus.wlstatus

object buffer {
  def flashsize_buffer(elem: buffer)(implicit _algebraic_implicit: algebraic.Algebraic): Int = {
    return ENCODED_NAT_SIZE + elem.length * 1
  }

  def encode_buffer(elem: buffer, index: Int, buf: buffer, nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    val tmpsize: Int = 0
    encode_nat(elem.length, index, buf, nbytes, err)
    if (err.get == types.error.ESUCCESS) {
      buf.copy(elem, 0, index + nbytes.get, elem.length)
      nbytes := nbytes.get + elem.length
    }
  }

  def decode_buffer(index: Int, buf: buffer, elem: buffer, nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    val tmpsize: Int = 0
    val decodedsize = Ref[Int](0)
    decode_nat(index, buf, decodedsize, nbytes, err)
    if (err.get == types.error.ESUCCESS) {
      elem.allocate(decodedsize.get, 0.toByte)
      elem.copy(buf, index + nbytes.get, 0, decodedsize.get)
      nbytes := nbytes.get + decodedsize.get
    }
  }
}
