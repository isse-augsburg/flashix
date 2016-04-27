// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package encoding

import encoding.data._
import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error
import types.file_mode.file_mode
import types.lpropflags.lpropflags
import types.seekflag.seekflag
import types.wlstatus.wlstatus

object buffer {
  def flashsize_buffer(elem: buffer)(implicit _algebraic_implicit: algebraic.Algebraic): Int = {
    return ENCODED_NAT_SIZE + elem.length * ENCODED_DATA_SIZE
  }

  def encode_buffer(elem: buffer, index: Int, buf: buffer, nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    val tmpsize = new Ref[Int](0)
    encode_nat(elem.length, index, buf, nbytes, err)
    if (err.get == types.error.ESUCCESS) {
      var arindex: Int = 0
      while (err.get == types.error.ESUCCESS && arindex < elem.length) {
        encode_data(elem(arindex), index + nbytes.get, buf, tmpsize, err)
        if (err.get == types.error.ESUCCESS) {
          nbytes := nbytes.get + tmpsize.get
          arindex = arindex + 1
        }
      }
    }
  }

  def decode_buffer(index: Int, buf: buffer, elem: buffer, nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    val tmpsize = new Ref[Int](0)
    val decodedsize = new Ref[Int](0)
    decode_nat(index, buf, decodedsize, nbytes, err)
    if (err.get == types.error.ESUCCESS) {
      elem.allocate(decodedsize.get, 0.toByte)
      var arindex: Int = 0
      while (err.get == types.error.ESUCCESS && arindex < decodedsize.get) {
        val d: Ref[Byte] = new Ref[Byte](elem(arindex))
        decode_data(index + nbytes.get, buf, d, tmpsize, err)
        elem(arindex) = d.get
        if (err.get == types.error.ESUCCESS) {
          nbytes := nbytes.get + tmpsize.get
          arindex = arindex + 1
        }
      }
    }
  }
}
