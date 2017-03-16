package encoding

import encoding.address._
import encoding.key._
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

object branch {
  def flashsize_branch(elem: branch)(implicit _algebraic_implicit: algebraic.Algebraic): Int = {
    if (elem.isInstanceOf[types.branch.mkbranch])
      return 1 + (flashsize_key(elem.key) + ENCODED_ADDRESS_SIZE)
    else     if (! elem.isInstanceOf[types.branch.mkbranch] && elem.isInstanceOf[types.branch.mkentry])
      return 1 + (flashsize_key(elem.key) + ENCODED_ADDRESS_SIZE)
    else
      return 1 + 0
  }

  def encode_branch(elem: branch, index: Int, buf: buffer, nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    nbytes := 1
    val tmpsize: Int = 0
    if (elem.isInstanceOf[types.branch.mkbranch]) {
      buf(index) = 0
      val tmpsize = Ref[Int](0)
      err := types.error.ESUCCESS
      if (err.get == types.error.ESUCCESS) {
        encode_key(elem.key, index + nbytes.get, buf, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        encode_address(elem.adr, index + nbytes.get, buf, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
    } else     if (elem.isInstanceOf[types.branch.mkentry]) {
      buf(index) = 1
      val tmpsize = Ref[Int](0)
      err := types.error.ESUCCESS
      if (err.get == types.error.ESUCCESS) {
        encode_key(elem.key, index + nbytes.get, buf, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        encode_address(elem.adr, index + nbytes.get, buf, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
    } else
      assert(false)
  }

  def decode_branch(index: Int, buf: buffer, elem: Ref[branch], nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    nbytes := 1
    err := types.error.ESUCCESS
    if (buf(index) == 0) {
      val tmpsize = Ref[Int](0)
      val key = Ref[key](types.key.uninit)
      val adr = Ref[address](types.address.uninit)
      if (err.get == types.error.ESUCCESS) {
        decode_key(index + nbytes.get, buf, key, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        decode_address(index + nbytes.get, buf, adr, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS)
        elem := types.branch.mkbranch(key.get, adr.get)
      
    } else     if (buf(index) == 1) {
      val tmpsize = Ref[Int](0)
      val key = Ref[key](types.key.uninit)
      val adr = Ref[address](types.address.uninit)
      if (err.get == types.error.ESUCCESS) {
        decode_key(index + nbytes.get, buf, key, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        decode_address(index + nbytes.get, buf, adr, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS)
        elem := types.branch.mkentry(key.get, adr.get)
      
    } else
      err := types.error.EINVAL
  }
}
