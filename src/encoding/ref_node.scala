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

object ref_node {
  def ENCODED_REF_NODE_SIZE(implicit _algebraic_implicit: algebraic.Algebraic): Int = {
    return ENCODED_NAT_SIZE
  }

  def encode_ref_node_empty(elem: ref_node, index: Int, buf: buffer, nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    nbytes := 0
    val tmpsize = Ref[Int](0)
    err := types.error.ESUCCESS
    if (err.get == types.error.ESUCCESS) {
      encode_nat(elem.leb, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
  }

  def decode_ref_node_empty(index: Int, buf: buffer, elem: Ref[ref_node], nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    nbytes := 0
    err := types.error.ESUCCESS
    val tmpsize = Ref[Int](0)
    val leb = Ref[Int](0)
    if (err.get == types.error.ESUCCESS) {
      decode_nat(index + nbytes.get, buf, leb, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS)
      elem := types.ref_node.rnode(leb.get)
    
  }
}
