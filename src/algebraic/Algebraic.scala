package algebraic

import helpers.scala._
import sorts._
import types._
import types.wlstatus.wlstatus

/**
 * algebraic operations
 *
 * implementation restrictions:
 * * algebraic operations must be deterministic (e.g., may not depend on mutable global state)
 * * algebraic operations may not use destructive operations
 * * results of algebraic operations may share memory with their input (but not with [parts of the] mutable global space)
 */
trait Algebraic {
  private implicit val _algebraic_implicit = this

  import helpers.scala.Int.plus1

  //
  // Implemented algebraic operations
  //

  def BRANCH_SIZE: Int = 2 * MIN_SIZE
  def LEB_SIZE: Int = PAGES_PER_LEB * EB_PAGE_SIZE
  def LOGLNUM: Int = 6
  def LPTLNUM: Int = 4
  def MAINLNUM: Int = 8
  def ORPHLNUM: Int = 2
  def SBLNUM: Int = 0

  def flashsize(gnd: group_node): Int = {
    2 * NODE_HEADER_SIZE + encoding.group_node.flashsize(gnd)
  }

  def flashsize(ind: index_node): Int = {
    2 * NODE_HEADER_SIZE + encoding.index_node.flashsize(ind)
  }

  def alignUp(n: Int, m: Int): Int = {
    assert(0 != m)
    if (n % m == 0) n else (n / m + 1) * m
  }

  def alignDown(n: Int, m: Int): Int = {
    assert(0 != m)
    if (n % m == 0) n else (n / m) * m
  }

  def below(NS: nat_set, m: Int): nat_set

  def datasize(buf: buffer): Int = {
    datasize(buf, buf.length)
  }

  def datasize(buf: buffer, FROM: Int): Int = FROM match {
    case plus1(n) => if (buf(n) == empty) datasize(buf, n) else n + 1
    case 0 => 0
  }

  def free_ecs(wlar: wlarray): nat_set = {
    free_ecs(wlar, wlar.length)
  }

  def free_ecs(wlar: wlarray, FROM: Int): nat_set = FROM match {
    case plus1(n) if n < wlar.length => free_ecs(wlar, n).union(if (wlar(n).status == types.wlstatus.free) new nat_set(wlar(n).ec) else new nat_set())
    case 0 => new nat_set()
  }

  def is_aligned(n: Int, m: Int): Boolean = {
    n % m == 0
  }

  def is_open(ino: Int, of: open_files): Boolean = {
    {???}
  }

  def isempty(buf: buffer): Boolean = {
    buf == mkempbuf(buf.length)
  }

  def isvalidtrailer(buf: buffer): Boolean = {
    buf == validtrailer
  }

  def load(br: branch): zbranch = br match {
    case types.branch.mkchecked(key) => types.zbranch.mkzchecked(key)
    case types.branch.mkentry(key, adr) => types.zbranch.mkzentry(key, adr, types.node_option.none)
    case types.branch.mkbranch(key, adr) => types.zbranch.mkzbranch(key, adr, null)
  }

  def max(n: Int, m: Int): Int = {
    if (n < m) m else n
  }

  def min(n: Int, m: Int): Int = {
    if (n < m) n else m
  }

  def min(n0: Int, n1: Int, n2: Int): Int = {
    min(n0, min(n1, n2))
  }

  def other(n: Int): Int = {
    if (n % 2 == 0) n + 1 else n - 1
  }

  def other_sb(sb: superblock): superblock = {
    types.superblock.mksb(sb.indexaddr, sb.maxino, other(sb.orph), other(sb.lpt), other(sb.log), sb.main)
  }

  def save(zbr: zbranch): branch = zbr match {
    case types.zbranch.mkzchecked(key) => types.branch.mkchecked(key)
    case types.zbranch.mkzentry(key, adr, nd_) => types.branch.mkentry(key, adr)
    case types.zbranch.mkzbranch(key, adr, r) => types.branch.mkbranch(key, adr)
  }

  def used_ecs(wlar: wlarray): nat_set = {
    used_ecs(wlar, wlar.length)
  }

  def used_ecs(wlar: wlarray, FROM: Int): nat_set = FROM match {
    case plus1(n) if n < wlar.length => used_ecs(wlar, n).union(if (wlar(n).status == types.wlstatus.used) new nat_set(wlar(n).ec) else new nat_set())
    case 0 => new nat_set()
  }



  //
  // Encoding operations
  //

  def pack_orphans(x: key_set): buffer = {
    val buf: Array[Byte] = new Array[Byte](LEB_SIZE)
    (helpers.scala.Encoding.encodeSet[key](_: key_set, _: Array[Byte], _: Int, encoding.key.encode _))(x, buf, 0)
    new ArrayWrapper[Byte](buf)
  }
  def unpack_orphans(buf: buffer): key_set = {
    assert(buf.length == LEB_SIZE)
    (helpers.scala.Encoding.decodeSetWrapper[key](_: Array[Byte], _: Int, encoding.key.decode))(buf.array, 0)._1
  }
  def is_orphans(buf: buffer): Boolean = try {
    unpack_orphans(buf)
    true
  } catch {
    case _: helpers.scala.DecodeFailure =>
      false
  }
  def pack_node_header(x: node_header): buffer = {
    val buf: Array[Byte] = new Array[Byte](NODE_HEADER_SIZE)
    encoding.node_header.encode(x, buf, 0)
    new ArrayWrapper[Byte](buf)
  }
  def unpack_node_header(buf: buffer): node_header = {
    assert(buf.length == NODE_HEADER_SIZE)
    encoding.node_header.decode(buf.array, 0)._1
  }
  def is_node_header(buf: buffer): Boolean = try {
    unpack_node_header(buf)
    true
  } catch {
    case _: helpers.scala.DecodeFailure =>
      false
  }
  def pack_rnd(x: ref_node): buffer = {
    val buf: Array[Byte] = new Array[Byte](EB_PAGE_SIZE)
    encoding.ref_node.encode(x, buf, 0)
    new ArrayWrapper[Byte](buf)
  }
  def unpack_rnd(buf: buffer): ref_node = {
    assert(buf.length == EB_PAGE_SIZE)
    encoding.ref_node.decode(buf.array, 0)._1
  }
  def is_refnode(buf: buffer): Boolean = try {
    unpack_rnd(buf)
    true
  } catch {
    case _: helpers.scala.DecodeFailure =>
      false
  }
  def pack_ind(x: index_node): buffer = {
    val buf: Array[Byte] = new Array[Byte](encoding.index_node.flashsize(x))
    encoding.index_node.encode(x, buf, 0)
    new ArrayWrapper[Byte](buf)
  }
  def unpack_ind(buf: buffer): index_node = {
    encoding.index_node.decode(buf.array, 0)._1
  }
  def is_indnode(buf: buffer): Boolean = try {
    unpack_ind(buf)
    true
  } catch {
    case _: helpers.scala.DecodeFailure =>
      false
  }
  def pack_gnd(x: group_node): buffer = {
    val buf: Array[Byte] = new Array[Byte](encoding.group_node.flashsize(x))
    encoding.group_node.encode(x, buf, 0)
    new ArrayWrapper[Byte](buf)
  }
  def unpack_gnd(buf: buffer): group_node = {
    encoding.group_node.decode(buf.array, 0)._1
  }
  def is_gndnode(buf: buffer): Boolean = try {
    unpack_gnd(buf)
    true
  } catch {
    case _: helpers.scala.DecodeFailure =>
      false
  }
  def pack_echdr(x: echeader): buffer = {
    val buf: Array[Byte] = new Array[Byte](EB_PAGE_SIZE)
    encoding.echeader.encode(x, buf, 0)
    new ArrayWrapper[Byte](buf)
  }
  def unpack_echdr(buf: buffer): echeader = {
    assert(buf.length == EB_PAGE_SIZE)
    encoding.echeader.decode(buf.array, 0)._1
  }
  def is_echdr(buf: buffer): Boolean = try {
    unpack_echdr(buf)
    true
  } catch {
    case _: helpers.scala.DecodeFailure =>
      false
  }
  def pack_vidhdr(x: vidheader): buffer = {
    val buf: Array[Byte] = new Array[Byte](EB_PAGE_SIZE)
    encoding.vidheader.encode(x, buf, 0)
    new ArrayWrapper[Byte](buf)
  }
  def unpack_vidhdr(buf: buffer): vidheader = {
    assert(buf.length == EB_PAGE_SIZE)
    encoding.vidheader.decode(buf.array, 0)._1
  }
  def is_vidhdr(buf: buffer): Boolean = try {
    unpack_vidhdr(buf)
    true
  } catch {
    case _: helpers.scala.DecodeFailure =>
      false
  }
  def pack_lpt(x: lp_array): buffer = {
    val buf: Array[Byte] = new Array[Byte](LEB_SIZE)
    (helpers.scala.Encoding.encodeArrayWrapperDeep[lprops](_: lp_array, _: Array[Byte], _: Int, encoding.lprops.encode))(x, buf, 0)
    new ArrayWrapper[Byte](buf)
  }
  def unpack_lpt(buf: buffer): lp_array = {
    assert(buf.length == LEB_SIZE)
    (helpers.scala.Encoding.decodeArrayWrapperDeep[lprops](_: Array[Byte], _: Int, encoding.lprops.decode))(buf.array, 0)._1
  }
  def is_lpt(buf: buffer): Boolean = try {
    unpack_lpt(buf)
    true
  } catch {
    case _: helpers.scala.DecodeFailure =>
      false
  }
  def pack_vtbl(x: vtbl): buffer = {
    val buf: Array[Byte] = new Array[Byte](LEB_SIZE)
    (helpers.scala.Encoding.encodeMapWrapper[Byte, Int](_: vtbl, _: Array[Byte], _:Int, helpers.scala.Encoding.encode, helpers.scala.Encoding.encode))(x, buf, 0)
    new ArrayWrapper[Byte](buf)
  }
  def unpack_vtbl(buf: buffer): vtbl = {
    assert(buf.length == LEB_SIZE)
    (helpers.scala.Encoding.decodeMapWrapper[Byte, Int](_: Array[Byte], _: Int, helpers.scala.Encoding.decodeByte, helpers.scala.Encoding.decodeNat))(buf.array, 0)._1
  }
  def pack_sb(x: superblock): buffer = {
    val buf: Array[Byte] = new Array[Byte](LEB_SIZE)
    encoding.superblock.encode(x, buf, 0)
    new ArrayWrapper[Byte](buf)
  }
  def unpack_sb(buf: buffer): superblock = {
    assert(buf.length == LEB_SIZE)
    encoding.superblock.decode(buf.array, 0)._1
  }
  def is_sb(buf: buffer): Boolean = try {
    unpack_sb(buf)
    true
  } catch {
    case _: helpers.scala.DecodeFailure =>
      false
  }

  //
  // Unimplemented algebraic operations, need to be implemented in a derived class
  //

  def EB_PAGE_SIZE: Int
  def MIN_SIZE: Int
  def NODE_HEADER_SIZE: Int
  def PAGES_PER_LEB: Int
  def ROOT_INO: Int
  def UBI_ERASE_RETRIES: Int
  def UBI_READ_RETRIES: Int
  def UBI_WRITE_RETRIES: Int
  def VFS_PAGE_SIZE: Int
  def VTBL_LNUM: Int
  def VTBL_VOLID: Byte
  def WL_THRESHOLD: Int
  def default_volid: Byte
  def empty: Byte
  def initial_ind: index_node
  def validtrailer: buffer
  def zero: Byte
  def zeropage: buffer

  def flashsize(param0: node): Int
  def max(param0: nat_set): Int
  def min(param0: nat_set): Int
  def <(param0: key, param1: key): Boolean
  def checksum(param0: buffer, param1: Int): Int
  def mkempbuf(param0: Int): buffer
  def mkzbuf(param0: Int): buffer
  def pr(param0: user, param1: metadata): Boolean
  def pw(param0: user, param1: metadata): Boolean
  def px(param0: user, param1: metadata): Boolean
  def to_vtbl(param0: volumes): vtbl
}
