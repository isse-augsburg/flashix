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

object metadata {
  def ENCODED_METADATA_SIZE(implicit _algebraic_implicit: algebraic.Algebraic): Int = {
    return (((((((ENCODED_NAT_SIZE + ENCODED_NAT_SIZE) + ENCODED_NAT_SIZE) + ENCODED_NAT_SIZE) + ENCODED_NAT_SIZE) + ENCODED_NAT_SIZE) + ENCODED_NAT_SIZE) + ENCODED_NAT_SIZE) + ENCODED_NAT_SIZE
  }

  def encode_metadata(elem: metadata, index: Int, buf: buffer, nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    nbytes := 0
    val tmpsize = Ref[Int](0)
    err := types.error.ESUCCESS
    if (err.get == types.error.ESUCCESS) {
      encode_nat(elem.mode, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_nat(elem.uid, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_nat(elem.gid, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_nat(elem.atime, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_nat(elem.atimesec, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_nat(elem.mtime, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_nat(elem.mtimesec, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_nat(elem.ctime, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_nat(elem.ctimesec, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
  }

  def decode_metadata(index: Int, buf: buffer, elem: Ref[metadata], nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    nbytes := 0
    err := types.error.ESUCCESS
    val tmpsize = Ref[Int](0)
    val mode = Ref[Int](0)
    val uid = Ref[Int](0)
    val gid = Ref[Int](0)
    val atime = Ref[Int](0)
    val atimesec = Ref[Int](0)
    val mtime = Ref[Int](0)
    val mtimesec = Ref[Int](0)
    val ctime = Ref[Int](0)
    val ctimesec = Ref[Int](0)
    if (err.get == types.error.ESUCCESS) {
      decode_nat(index + nbytes.get, buf, mode, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_nat(index + nbytes.get, buf, uid, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_nat(index + nbytes.get, buf, gid, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_nat(index + nbytes.get, buf, atime, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_nat(index + nbytes.get, buf, atimesec, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_nat(index + nbytes.get, buf, mtime, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_nat(index + nbytes.get, buf, mtimesec, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_nat(index + nbytes.get, buf, ctime, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_nat(index + nbytes.get, buf, ctimesec, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS)
      elem := types.metadata.mkmetadata(mode.get, uid.get, gid.get, atime.get, atimesec.get, mtime.get, mtimesec.get, ctime.get, ctimesec.get)
    
  }
}
