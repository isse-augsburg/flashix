// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package algebraic

import encoding.group_node._
import encoding.index_node._
import encoding.lprops._
import encoding.node_header._
import encoding.superblock._
import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
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

  //
  // Implemented algebraic operations
  //

  def BRANCH_SIZE: Int = 2 * MIN_SIZE
  def LEB_SIZE: Int = PAGES_PER_LEB * EB_PAGE_SIZE
  def SB_LOG: Int = 1
  def SB_LPT: Int = 5
  def SB_ORPH: Int = 3
  def zeropage: buffer = mkzbuf(VFS_PAGE_SIZE)

  def alignedsize(gnd: group_node): Int = {
    return alignUp(group_node_size_headerless(gnd), 2 * NODE_HEADER_SIZE)
  }

  def alignedsize(ind: index_node): Int = {
    return alignUp(index_node_size_headerless(ind), 2 * NODE_HEADER_SIZE)
  }

  def flashsize(gnd: group_node): Int = {
    return 2 * NODE_HEADER_SIZE + alignedsize(gnd)
  }

  def flashsize(ind: index_node): Int = {
    return 2 * NODE_HEADER_SIZE + alignedsize(ind)
  }

  def alignUp(n: Int, m: Int): Int = {
    if (n % m == 0)
      return n
    else
      return (n / m + 1) * m
  }

  def alignDown(n: Int, m: Int): Int = {
    if (n % m == 0)
      return n
    else
      return (n / m) * m
  }

  def datasize(buf: buffer): Int = {
    return datasize(buf, buf.length)
  }

  def datasize(buf: buffer, ino: Int): Int = {
    if (ino == 0)
      return 0
    else     if (buf(ino - 1) == empty)
      return datasize(buf, ino - 1)
    else
      return (ino - 1) + 1
  }

  def free_ecs(wlar: wlarray): nat_set = {
    return free_ecs(wlar, wlar.length)
  }

  def free_ecs(wlar: wlarray, ino: Int): nat_set = {
    if (ino == 0)
      return new nat_set()
    else     if (wlar(ino - 1).status == types.wlstatus.free) {
      val ns: nat_set = free_ecs(wlar, ino - 1).deepCopy
      ns += wlar(ino - 1).ec
      return ns
    } else
      return free_ecs(wlar, ino - 1)
  }

  def is_aligned(n: Int, m: Int): Boolean = {
    return n % m == 0
  }

  def isempty(buf: buffer): Boolean = {
    return isempty(buf, 0, buf.length)
  }

  def isempty(buf: buffer, n: Int, ino: Int): Boolean = {
    if (ino == 0)
      return true
    else
      return buf(n + (ino - 1)) == empty && isempty(buf, n, ino - 1)
  }

  def load(br: branch): zbranch = {
    br match {
      case types.branch.mkbranch(key, adr) =>
        return types.zbranch.mkzbranch(key, adr, null)
      case types.branch.mkentry(key, adr) =>
        return types.zbranch.mkzentry(key, adr, types.node_option.none)
      case types.branch.mkchecked(key) =>
        return types.zbranch.mkzchecked(key)
    }
  }

  def lptlebs(mainareasize: Int): Int = {
    return lptsize(mainareasize) / LEB_SIZE
  }

  def lptsize(mainareasize: Int): Int = {
    return alignUp(mainareasize * ENCODED_LPROPS_SIZE, LEB_SIZE)
  }

  def max(n: Int, m: Int): Int = {
    if (n < m)
      return m
    else
      return n
  }

  def min(n: Int, m: Int): Int = {
    if (n < m)
      return n
    else
      return m
  }

  def min(n0: Int, n1: Int, n2: Int): Int = {
    return min(n0, min(n1, n2))
  }

  def mkempbuf(n: Int): buffer = {
    val buf: buffer = new buffer(n).fill(0.toByte)
    val buf0: buffer = buf
    buf0.fill(empty)
    return buf0
  }

  def mkzbuf(n: Int): buffer = {
    val buf: buffer = new buffer(n).fill(0.toByte)
    val buf0: buffer = buf
    buf0.fill(zero)
    return buf0
  }

  def rangeeq(buf: buffer, n0: Int, buf0: buffer, n1: Int, ino: Int): Boolean = {
    if (ino == 0)
      return n0 <= buf.length && n1 <= buf0.length
    else
      return buf(n0) == buf0(n1) && rangeeq(buf, n0 + 1, buf0, n1 + 1, ino - 1)
  }

  def save(zbr: zbranch): branch = {
    zbr match {
      case types.zbranch.mkzbranch(key, adr, r) =>
        return types.branch.mkbranch(key, adr)
      case types.zbranch.mkzentry(key, adr, nd_) =>
        return types.branch.mkentry(key, adr)
      case types.zbranch.mkzchecked(key) =>
        return types.branch.mkchecked(key)
    }
  }

  def size(sb: superblock): Int = {
    return alignUp(size_unaligned(sb), EB_PAGE_SIZE)
  }

  def used_ecs(wlar: wlarray): nat_set = {
    return used_ecs(wlar, wlar.length)
  }

  def used_ecs(wlar: wlarray, ino: Int): nat_set = {
    if (ino == 0)
      return new nat_set()
    else     if (wlar(ino - 1).status == types.wlstatus.used) {
      val ns: nat_set = used_ecs(wlar, ino - 1).deepCopy
      ns += wlar(ino - 1).ec
      return ns
    } else
      return used_ecs(wlar, ino - 1)
  }



  //
  // Unimplemented algebraic operations, need to be implemented in a derived class
  //

  def EB_PAGE_SIZE: Int
  def MIN_SIZE: Int
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
  def validtrailer: buffer
  def zero: Byte

  def flashsize(param0: group_node_list): Int
  def flashsize(param0: node): Int
  def keys(param0: nat_set): key_set
  def max(param0: nat_set): Int
  def min(param0: nat_set): Int
  def toStr(param0: Int): String
  def <(param0: key, param1: key): Boolean
  def at(param0: address_list, param1: Int): address
  def below(param0: nat_set, param1: Int): nat_set
  def checksum(param0: buffer, param1: Int): Int
  def is_open(param0: Int, param1: open_files): Boolean
  def minus(param0: nat_set, param1: Int): nat_set
  def pr(param0: Byte, param1: metadata): Boolean
  def pw(param0: Byte, param1: metadata): Boolean
  def px(param0: Byte, param1: metadata): Boolean
  def to_vtbl(param0: volumes): vtbl
  def ⊑(param0: path, param1: path): Boolean
}
