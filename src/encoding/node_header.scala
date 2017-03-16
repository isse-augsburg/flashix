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

object node_header {
  def NODE_HEADER_SIZE(implicit _algebraic_implicit: algebraic.Algebraic): Int = {
    return ENCODED_NAT_SIZE + ENCODED_BOOL_SIZE
  }

  def encode_header_empty(elem: node_header, index: Int, buf: buffer, nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    nbytes := 0
    val tmpsize = Ref[Int](0)
    err := types.error.ESUCCESS
    if (err.get == types.error.ESUCCESS) {
      encode_nat(elem.size, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_bool(elem.ispadding, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
  }

  def decode_header_empty(index: Int, buf: buffer, elem: Ref[node_header], nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    nbytes := 0
    err := types.error.ESUCCESS
    val tmpsize = Ref[Int](0)
    val size = Ref[Int](0)
    val ispadding = Ref[Boolean](helpers.scala.Boolean.uninit)
    if (err.get == types.error.ESUCCESS) {
      decode_nat(index + nbytes.get, buf, size, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_bool(index + nbytes.get, buf, ispadding, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS)
      elem := types.node_header.nodeheader(size.get, ispadding.get)
    
  }
}
