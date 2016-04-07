// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package encoding

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error
import types.file_mode.file_mode
import types.lpropflags.lpropflags
import types.seekflag.seekflag
import types.wlstatus.wlstatus

object key {
  def flashsize_key(elem: key)(implicit _algebraic_implicit: algebraic.Algebraic): Int = {
    if (elem.isInstanceOf[types.key.inodekey])
      return 1 + ENCODED_NAT_SIZE
    else     if (! elem.isInstanceOf[types.key.inodekey] && elem.isInstanceOf[types.key.datakey])
      return 1 + (ENCODED_NAT_SIZE + ENCODED_NAT_SIZE)
    else     if (! elem.isInstanceOf[types.key.inodekey] && (! elem.isInstanceOf[types.key.datakey] && elem.isInstanceOf[types.key.dentrykey]))
      return 1 + (ENCODED_NAT_SIZE + flashsize_string(elem.name))
    else
      return 1 + 0
  }

  def encode_key(elem: key, index: Int, buf: buffer, nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    nbytes := 1
    val tmpsize: Int = 0
    if (elem.isInstanceOf[types.key.inodekey]) {
      buf(index) = 0
      val tmpsize = new Ref[Int](0)
      err := types.error.ESUCCESS
      if (err.get == types.error.ESUCCESS) {
        encode_nat(elem.ino, index + nbytes.get, buf, tmpsize, err)
        assert(tmpsize.get == ENCODED_NAT_SIZE, """encoding has invalid size""")
        nbytes := nbytes.get + tmpsize.get
      }
    } else     if (elem.isInstanceOf[types.key.datakey]) {
      buf(index) = 1
      val tmpsize = new Ref[Int](0)
      err := types.error.ESUCCESS
      if (err.get == types.error.ESUCCESS) {
        encode_nat(elem.ino, index + nbytes.get, buf, tmpsize, err)
        assert(tmpsize.get == ENCODED_NAT_SIZE, """encoding has invalid size""")
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        encode_nat(elem.part, index + nbytes.get, buf, tmpsize, err)
        assert(tmpsize.get == ENCODED_NAT_SIZE, """encoding has invalid size""")
        nbytes := nbytes.get + tmpsize.get
      }
    } else     if (elem.isInstanceOf[types.key.dentrykey]) {
      buf(index) = 2
      val tmpsize = new Ref[Int](0)
      err := types.error.ESUCCESS
      if (err.get == types.error.ESUCCESS) {
        encode_nat(elem.ino, index + nbytes.get, buf, tmpsize, err)
        assert(tmpsize.get == ENCODED_NAT_SIZE, """encoding has invalid size""")
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        encode_string(elem.name, index + nbytes.get, buf, tmpsize, err)
        assert(tmpsize.get == flashsize_string(elem.name), """encoding has invalid size""")
        nbytes := nbytes.get + tmpsize.get
      }
    } else
      assert(false)
    assert(nbytes.get == flashsize_key(elem), """encoding has invalid size""")
  }

  def decode_key(index: Int, buf: buffer, elem: Ref[key], nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    nbytes := 1
    err := types.error.ESUCCESS
    if (buf(index) == 0) {
      val tmpsize = new Ref[Int](0)
      val ino = new Ref[Int](0)
      if (err.get == types.error.ESUCCESS) {
        decode_nat(index + nbytes.get, buf, ino, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS)
        elem := types.key.inodekey(ino.get)
      
    } else     if (buf(index) == 1) {
      val tmpsize = new Ref[Int](0)
      val ino = new Ref[Int](0)
      val part = new Ref[Int](0)
      if (err.get == types.error.ESUCCESS) {
        decode_nat(index + nbytes.get, buf, ino, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        decode_nat(index + nbytes.get, buf, part, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS)
        elem := types.key.datakey(ino.get, part.get)
      
    } else     if (buf(index) == 2) {
      val tmpsize = new Ref[Int](0)
      val ino = new Ref[Int](0)
      val name = new Ref[String]("")
      if (err.get == types.error.ESUCCESS) {
        decode_nat(index + nbytes.get, buf, ino, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        decode_string(index + nbytes.get, buf, name, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS)
        elem := types.key.dentrykey(ino.get, name.get)
      
    } else
      err := types.error.EINVAL
  }
}
