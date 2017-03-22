// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package encoding

import encoding.branch_array._
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

object index_node {
  def index_node_size_headerless(elem: index_node)(implicit _algebraic_implicit: algebraic.Algebraic): Int = {
    return (flashsize_branch_array(elem.branches) + ENCODED_BOOL_SIZE) + ENCODED_NAT_SIZE
  }

  def encode_index_node_headerless(elem: index_node, index: Int, buf: buffer, nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    nbytes := 0
    val tmpsize = Ref[Int](0)
    err := types.error.ESUCCESS
    if (err.get == types.error.ESUCCESS) {
      encode_branch_array(elem.branches, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_bool(elem.leaf, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_nat(elem.usedsize, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
  }

  def decode_index_node_headerless(index: Int, buf: buffer, elem: Ref[index_node], nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    nbytes := 0
    err := types.error.ESUCCESS
    val tmpsize = Ref[Int](0)
    val branches: branch_array = new branch_array()
    val leaf = Ref[Boolean](helpers.scala.Boolean.uninit)
    val usedsize = Ref[Int](0)
    if (err.get == types.error.ESUCCESS) {
      decode_branch_array(index + nbytes.get, buf, branches, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_bool(index + nbytes.get, buf, leaf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_nat(index + nbytes.get, buf, usedsize, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS)
      elem := types.index_node.indexnode(branches, leaf.get, usedsize.get).deepCopy
    
  }
}
