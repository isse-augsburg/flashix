// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package encoding

import encoding.address._
import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error
import types.file_mode.file_mode
import types.lpropflags.lpropflags
import types.seekflag.seekflag
import types.wlstatus.wlstatus

object superblock {
  def size_unaligned(elem: superblock)(implicit _algebraic_implicit: algebraic.Algebraic): Int = {
    return ENCODED_SUPERBLOCK_SIZE
  }

  def ENCODED_SUPERBLOCK_SIZE(implicit _algebraic_implicit: algebraic.Algebraic): Int = {
    return (((((ENCODED_ADDRESS_SIZE + ENCODED_NAT_SIZE) + ENCODED_NAT_SIZE) + ENCODED_NAT_SIZE) + ENCODED_NAT_SIZE) + ENCODED_NAT_SIZE) + ENCODED_NAT_SIZE
  }

  def encode_superblock_unaligned(elem: superblock, index: Int, buf: buffer, nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    nbytes := 0
    val tmpsize = new Ref[Int](0)
    err := types.error.ESUCCESS
    if (err.get == types.error.ESUCCESS) {
      encode_address(elem.indexaddr, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_nat(elem.maxino, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_nat(elem.log, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_nat(elem.orph, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_nat(elem.orphsize, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_nat(elem.lpt, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      encode_nat(elem.main, index + nbytes.get, buf, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
  }

  def decode_superblock_unaligned(index: Int, buf: buffer, elem: superblock, nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    nbytes := 0
    err := types.error.ESUCCESS
    val tmpsize = new Ref[Int](0)
    val indexaddr = new Ref[address](types.address.uninit)
    val maxino = new Ref[Int](0)
    val log = new Ref[Int](0)
    val orph = new Ref[Int](0)
    val orphsize = new Ref[Int](0)
    val lpt = new Ref[Int](0)
    val main = new Ref[Int](0)
    if (err.get == types.error.ESUCCESS) {
      decode_address(index + nbytes.get, buf, indexaddr, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_nat(index + nbytes.get, buf, maxino, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_nat(index + nbytes.get, buf, log, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_nat(index + nbytes.get, buf, orph, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_nat(index + nbytes.get, buf, orphsize, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_nat(index + nbytes.get, buf, lpt, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS) {
      decode_nat(index + nbytes.get, buf, main, tmpsize, err)
      nbytes := nbytes.get + tmpsize.get
    }
    if (err.get == types.error.ESUCCESS)
      elem := types.superblock.mksb(indexaddr.get, maxino.get, log.get, orph.get, orphsize.get, lpt.get, main.get)
    
  }
}
