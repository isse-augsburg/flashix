// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package encoding

import encoding.node._
import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error
import types.file_mode.file_mode
import types.lpropflags.lpropflags
import types.seekflag.seekflag
import types.wlstatus.wlstatus

object group_node {
  def group_node_size_headerless(elem: group_node)(implicit _algebraic_implicit: algebraic.Algebraic): Int = {
    return ((flashsize_node(elem.nd) + ENCODED_NAT_SIZE) + ENCODED_BOOL_SIZE) + ENCODED_BOOL_SIZE
  }

  def encode_group_node_headerless(elem: group_node, index: Int, buf: buffer, nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    nbytes := 0
    val tmpsize = new Ref[Int](0)
    err := types.error.ESUCCESS
    if (err.get == types.error.ESUCCESS) {
      encode_node(elem.nd, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_nat(elem.sqnum, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_bool(elem.start, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_bool(elem.end, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
  }

  def decode_group_node_headerless(index: Int, buf: buffer, elem: Ref[group_node], nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    nbytes := 0
    err := types.error.ESUCCESS
    val tmpsize = new Ref[Int](0)
    val nd = new Ref[node](types.node.uninit)
    val sqnum = new Ref[Int](0)
    val start = new Ref[Boolean](helpers.scala.Boolean.uninit)
    val end = new Ref[Boolean](helpers.scala.Boolean.uninit)
    if (err.get == types.error.ESUCCESS) {
      decode_node(index + nbytes.get, buf, nd, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_nat(index + nbytes.get, buf, sqnum, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_bool(index + nbytes.get, buf, start, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_bool(index + nbytes.get, buf, end, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS)
      elem := types.group_node.mkgnode(nd.get, sqnum.get, start.get, end.get).deepCopy
    
  }
}
