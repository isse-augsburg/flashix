package integration

import types._
import types.key._
import sorts._

final class Algebraic(val mtd: MTDSimulation) extends algebraic.Algebraic {

  override def below(NS: nat_set, m: Int): nat_set = {
    val result = new nat_set()
    NS.set.foreach { elem =>
      if (elem < m)
        result += elem
    }
    result
  }

  override def is_open(ino: Int, of: open_files): Boolean = of.map.exists { _._2.ino == ino }

  //
  // Unimplemented algebraic operations
  //

  override def EB_PAGE_SIZE: Int = mtd.EB_PAGE_SIZE
  override def MIN_SIZE: Int = 4
  override def NODE_HEADER_SIZE: Int = 8 // space for 1 nat and one bool = 5 bytes, however, we need a value where NODE_HEADER_SIZE divides EB_PAGE_SIZE
  override def PAGES_PER_LEB: Int = mtd.PAGES_PER_PEB - 2
  override def ROOT_INO: Int = 1
  override def UBI_ERASE_RETRIES: Int = 5
  override def UBI_READ_RETRIES: Int = 5
  override def UBI_WRITE_RETRIES: Int = 5
  override def VFS_PAGE_SIZE: Int = 4 * 1024
  override def VTBL_LNUM: Int = 0
  override def VTBL_VOLID: Byte = 0xFF.toByte
  override def WL_THRESHOLD: Int = 100
  override def default_volid: Byte = 0
  override def empty: Byte = mtd.empty
  override def initial_ind: index_node = {
    val brancharray = new branch_array(BRANCH_SIZE)
    brancharray.fill(branch.uninit)
    index_node(brancharray, true, 0)
  }
  override def validtrailer: buffer = mkzbuf(NODE_HEADER_SIZE)
  override def zero: Byte = 0
  override def zeropage: buffer = mkzbuf(VFS_PAGE_SIZE)

  override def flashsize(param0: node): Int = {
    var flashsize = encoding.group_node.flashsize(group_node.mkgnode(param0, 0, true, true))(this)
    if (flashsize % (2 * NODE_HEADER_SIZE) != 0) {
      flashsize -= flashsize % (2 * NODE_HEADER_SIZE)
      flashsize += 2 * NODE_HEADER_SIZE
    }
    2 * NODE_HEADER_SIZE + flashsize
  }
  override def max(param0: nat_set): Int = param0.set.max
  override def min(param0: nat_set): Int = param0.set.min
  override def <(param0: key, param1: key): Boolean = param0 match {
    case inodekey(ino0) =>
      ino0 < param1.ino ||
      (! param1.isInstanceOf[inodekey] && ino0 == param1.ino)
    case dentrykey(ino0, name0) =>
      ino0 < param1.ino ||
      (param1.isInstanceOf[dentrykey] && ino0 == param1.ino && name0 < param1.name) ||
      (param1.isInstanceOf[datakey] && ino0 == param1.ino)
    case datakey(ino0, part0) =>
      ino0 < param1.ino ||
      (param1.isInstanceOf[datakey] && ino0 == param1.ino && part0 < param1.part)
  }
  override def checksum(param0: buffer, param1: Int): Int = 0
  override def mkempbuf(param0: Int): buffer = {
    val buf = new buffer(param0)
    (0 until param0).foreach { index =>
      buf(index) = empty
    }
    buf
  }
  override def mkzbuf(param0: Int): buffer = {
    val buf = new buffer(param0)
    (0 until param0).foreach { index =>
      buf(index) = zero
    }
    buf
  }
  override def pr(param0: user, param1: metadata): Boolean = true
  override def pw(param0: user, param1: metadata): Boolean = true
  override def px(param0: user, param1: metadata): Boolean = true
  override def to_vtbl(param0: volumes): vtbl = {
    val vtbl = new vtbl()
    param0.map.foreach { case (volumeid, vol) =>
      vtbl += volumeid -> vol.length
    }
    vtbl
  }

  // Fix encoding

  override def is_refnode(buf: buffer): Boolean = try {
    if (is_empty(buf))
      false
    else {
      unpack_rnd(buf)
      true
    }
  } catch {
    case _: helpers.scala.DecodeFailure =>
      false
  }

  override def is_node_header(buf: buffer): Boolean = try {
    if (is_empty(buf))
      false
    else {
      unpack_node_header(buf)
      true
    }
  } catch {
    case _: helpers.scala.DecodeFailure =>
      false
  }

  def is_empty(buf: buffer): Boolean = {
    (0 until buf.length).forall { i =>
      buf(i) == empty
    }
  }
}
