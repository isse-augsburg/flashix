package encoding

import encoding.buffer._
import encoding.key._
import encoding.metadata._
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

object node {
  def flashsize_node(elem: node)(implicit _algebraic_implicit: algebraic.Algebraic): Int = {
    if (elem.isInstanceOf[types.node.inodenode])
      return 1 + (((((flashsize_key(elem.key) + ENCODED_METADATA_SIZE) + ENCODED_BOOL_SIZE) + ENCODED_NAT_SIZE) + ENCODED_NAT_SIZE) + ENCODED_NAT_SIZE)
    else     if (! elem.isInstanceOf[types.node.inodenode] && elem.isInstanceOf[types.node.dentrynode])
      return 1 + (flashsize_key(elem.key) + ENCODED_NAT_SIZE)
    else     if (! elem.isInstanceOf[types.node.inodenode] && (! elem.isInstanceOf[types.node.dentrynode] && elem.isInstanceOf[types.node.datanode]))
      return 1 + (flashsize_key(elem.key) + flashsize_buffer(elem.data))
    else     if (! elem.isInstanceOf[types.node.inodenode] && (! elem.isInstanceOf[types.node.dentrynode] && (! elem.isInstanceOf[types.node.datanode] && elem.isInstanceOf[types.node.truncnode])))
      return 1 + (flashsize_key(elem.key) + ENCODED_NAT_SIZE)
    else
      return 1 + 0
  }

  def encode_node(elem: node, index: Int, buf: buffer, nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    nbytes := 1
    val tmpsize: Int = 0
    if (elem.isInstanceOf[types.node.inodenode]) {
      buf(index) = 0
      val tmpsize = Ref[Int](0)
      err := types.error.ESUCCESS
      if (err.get == types.error.ESUCCESS) {
        encode_key(elem.key, index + nbytes.get, buf, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        encode_metadata(elem.meta, index + nbytes.get, buf, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        encode_bool(elem.directory, index + nbytes.get, buf, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        encode_nat(elem.nlink, index + nbytes.get, buf, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        encode_nat(elem.nsubdirs, index + nbytes.get, buf, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        encode_nat(elem.size, index + nbytes.get, buf, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
    } else     if (elem.isInstanceOf[types.node.dentrynode]) {
      buf(index) = 1
      val tmpsize = Ref[Int](0)
      err := types.error.ESUCCESS
      if (err.get == types.error.ESUCCESS) {
        encode_key(elem.key, index + nbytes.get, buf, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        encode_nat(elem.ino, index + nbytes.get, buf, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
    } else     if (elem.isInstanceOf[types.node.datanode]) {
      buf(index) = 2
      val tmpsize = Ref[Int](0)
      err := types.error.ESUCCESS
      if (err.get == types.error.ESUCCESS) {
        encode_key(elem.key, index + nbytes.get, buf, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        encode_buffer(elem.data, index + nbytes.get, buf, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
    } else     if (elem.isInstanceOf[types.node.truncnode]) {
      buf(index) = 3
      val tmpsize = Ref[Int](0)
      err := types.error.ESUCCESS
      if (err.get == types.error.ESUCCESS) {
        encode_key(elem.key, index + nbytes.get, buf, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        encode_nat(elem.size, index + nbytes.get, buf, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
    } else
      assert(false)
  }

  def decode_node(index: Int, buf: buffer, elem: Ref[node], nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    nbytes := 1
    err := types.error.ESUCCESS
    if (buf(index) == 0) {
      val tmpsize = Ref[Int](0)
      val key = Ref[key](types.key.uninit)
      val meta = Ref[metadata](types.metadata.uninit)
      val directory = Ref[Boolean](helpers.scala.Boolean.uninit)
      val nlink = Ref[Int](0)
      val nsubdirs = Ref[Int](0)
      val size = Ref[Int](0)
      if (err.get == types.error.ESUCCESS) {
        decode_key(index + nbytes.get, buf, key, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        decode_metadata(index + nbytes.get, buf, meta, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        decode_bool(index + nbytes.get, buf, directory, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        decode_nat(index + nbytes.get, buf, nlink, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        decode_nat(index + nbytes.get, buf, nsubdirs, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        decode_nat(index + nbytes.get, buf, size, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS)
        elem := types.node.inodenode(key.get, meta.get, directory.get, nlink.get, nsubdirs.get, size.get)
      
    } else     if (buf(index) == 1) {
      val tmpsize = Ref[Int](0)
      val key = Ref[key](types.key.uninit)
      val ino = Ref[Int](0)
      if (err.get == types.error.ESUCCESS) {
        decode_key(index + nbytes.get, buf, key, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        decode_nat(index + nbytes.get, buf, ino, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS)
        elem := types.node.dentrynode(key.get, ino.get)
      
    } else     if (buf(index) == 2) {
      val tmpsize = Ref[Int](0)
      val key = Ref[key](types.key.uninit)
      val data: buffer = new buffer()
      if (err.get == types.error.ESUCCESS) {
        decode_key(index + nbytes.get, buf, key, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        decode_buffer(index + nbytes.get, buf, data, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS)
        elem := types.node.datanode(key.get, data).deepCopy
      
    } else     if (buf(index) == 3) {
      val tmpsize = Ref[Int](0)
      val key = Ref[key](types.key.uninit)
      val size = Ref[Int](0)
      if (err.get == types.error.ESUCCESS) {
        decode_key(index + nbytes.get, buf, key, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS) {
        decode_nat(index + nbytes.get, buf, size, tmpsize, err)
        nbytes := nbytes.get + tmpsize.get
      }
      if (err.get == types.error.ESUCCESS)
        elem := types.node.truncnode(key.get, size.get)
      
    } else
      err := types.error.EINVAL
  }
}
