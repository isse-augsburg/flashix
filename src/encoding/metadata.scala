// Flashix: a verified file system for flash memory
// (c) 2015-2018 Institute for Software & Systems Engineering <http://isse.de/flashix>
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

object metadata {
  def flashsize_metadata(elem: metadata)(implicit _algebraic_implicit: algebraic.Algebraic): Int = {
    return (((((((flashsize_int(elem.mode) + flashsize_int(elem.uid)) + flashsize_int(elem.gid)) + flashsize_int(elem.atime)) + flashsize_int(elem.atimesec)) + flashsize_int(elem.mtime)) + flashsize_int(elem.mtimesec)) + flashsize_int(elem.ctime)) + flashsize_int(elem.ctimesec)
  }

  def encode_metadata(elem: metadata, index: Int, buf: buffer, nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    nbytes := 0
    val tmpsize = Ref[Int](0)
    err := types.error.ESUCCESS
    if (err.get == types.error.ESUCCESS) {
      encode_int(elem.mode, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_int(elem.uid, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_int(elem.gid, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_int(elem.atime, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_int(elem.atimesec, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_int(elem.mtime, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_int(elem.mtimesec, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_int(elem.ctime, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_int(elem.ctimesec, index + nbytes.get, buf, tmpsize, err)
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
      decode_int(index + nbytes.get, buf, mode, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_int(index + nbytes.get, buf, uid, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_int(index + nbytes.get, buf, gid, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_int(index + nbytes.get, buf, atime, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_int(index + nbytes.get, buf, atimesec, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_int(index + nbytes.get, buf, mtime, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_int(index + nbytes.get, buf, mtimesec, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_int(index + nbytes.get, buf, ctime, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_int(index + nbytes.get, buf, ctimesec, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS)
      elem := types.metadata.mkmetadata(mode.get, uid.get, gid.get, atime.get, atimesec.get, mtime.get, mtimesec.get, ctime.get, ctimesec.get)
    
  }
}
