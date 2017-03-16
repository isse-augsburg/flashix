package encoding

import encoding.branch._
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

object branch_array {
  def flashsize_branch_array(elem: branch_array)(implicit _algebraic_implicit: algebraic.Algebraic): Int = {
    return ENCODED_NAT_SIZE + flashsize_branch_array_rec(elem, elem.length)
  }

  def flashsize_branch_array_rec(ar: branch_array, upto: Int)(implicit _algebraic_implicit: algebraic.Algebraic): Int = {
    if (upto == 0)
      return 0
    else
      return flashsize_branch(ar(upto - 1)) + flashsize_branch_array_rec(ar, upto - 1)
  }

  def encode_branch_array(elem: branch_array, index: Int, buf: buffer, nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    val tmpsize = Ref[Int](0)
    encode_nat(elem.length, index, buf, nbytes, err)
    if (err.get == types.error.ESUCCESS) {
      var arindex: Int = 0
      while (err.get == types.error.ESUCCESS && arindex < elem.length) {
        encode_branch(elem(arindex), index + nbytes.get, buf, tmpsize, err)
        if (err.get == types.error.ESUCCESS) {
          nbytes := nbytes.get + tmpsize.get
          arindex = arindex + 1
        }
      }
    }
  }

  def decode_branch_array(index: Int, buf: buffer, elem: branch_array, nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    val tmpsize = Ref[Int](0)
    val decodedsize = Ref[Int](0)
    decode_nat(index, buf, decodedsize, nbytes, err)
    if (err.get == types.error.ESUCCESS) {
      elem.allocate(decodedsize.get, types.branch.uninit)
      var arindex: Int = 0
      while (err.get == types.error.ESUCCESS && arindex < decodedsize.get) {
        
        {
          val br: Ref[branch] = Ref[branch](elem(arindex))
          decode_branch(index + nbytes.get, buf, br, tmpsize, err)
          elem(arindex) = br.get
        }
        if (err.get == types.error.ESUCCESS) {
          nbytes := nbytes.get + tmpsize.get
          arindex = arindex + 1
        }
      }
    }
  }
}
