// Flashix: a verified file system for flash memory
// (c) 2015-2018 Institute for Software & Systems Engineering <http://isse.de/flashix>
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
import java.util.concurrent.locks._
import types._

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
  def SB_LOG: Int = 1
  def SB_LPT: Int = 5
  def SB_ORPH: Int = 3
  def zeropage: buffer = mkzbuf(VFS_PAGE_SIZE)

  def alignedsize(nd: group_node): Int = {
    return alignUp(group_node_size_headerless(nd), 2 * NODE_HEADER_SIZE)
  }

  def alignedsize(nd: index_node): Int = {
    return alignUp(index_node_size_headerless(nd), 2 * NODE_HEADER_SIZE)
  }

  def flashsize(nd: group_node): Int = {
    return 2 * NODE_HEADER_SIZE + alignedsize(nd)
  }

  def flashsize(nd: index_node): Int = {
    return 2 * NODE_HEADER_SIZE + alignedsize(nd)
  }

  def keys(nat_set_variable0: nat_set): key_set = {
    if (nat_set_variable0.isEmpty)
      return new key_set()
    else {
      val key_set_variable0: key_set = keys(nat_set_variable0.tail).deepCopy
      key_set_variable0 += types.key.inodekey(nat_set_variable0.head)
      return key_set_variable0
    }
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

  def encoding_size(a: superblock, m: Int): Int = {
    return alignUp(encoding_size_unaligned(a), m)
  }

  def is_aligned(n: Int, m: Int): Boolean = {
    return n % m == 0
  }

  def load(branch_variable0: branch): zbranch = {
    branch_variable0 match {
      case types.branch.mkbranch(key, adr) =>
        return types.zbranch.mkzbranch(key, adr, null)
      case types.branch.mkentry(key, adr) =>
        return types.zbranch.mkzentry(key, adr)
    }
  }

  def lptlebs(mainareasize: Int, n: Int): Int = {
    return lptsize(mainareasize, n) / n
  }

  def lptsize(mainareasize: Int, n: Int): Int = {
    return alignUp(mainareasize * ENCODED_LPROPS_SIZE, n)
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

  def mkzbuf(n: Int): buffer = {
    val buffer_variable0: buffer = new buffer(n).fill(0.toByte)
    val buffer_variable1: buffer = buffer_variable0
    buffer_variable1.fill(zero)
    return buffer_variable1
  }

  def save(zbranch_variable0: zbranch): branch = {
    zbranch_variable0 match {
      case types.zbranch.mkzbranch(key, adr, r) =>
        return types.branch.mkbranch(key, adr)
      case types.zbranch.mkzentry(key, adr) =>
        return types.branch.mkentry(key, adr)
    }
  }

  def save(znd: znode): index_node = {
    return types.index_node.indexnode(save(znd.zbranches, 0, znd.usedsize), znd.leaf, znd.usedsize)
  }

  def save(zbrar: zbranch_array, m: Int, mode: Int): branch_array = {
    if (mode == 0)
      return new branch_array(BRANCH_SIZE).fill(types.branch.uninit)
    else {
      val branch_array_variable0: branch_array = save(zbrar, m + 1, mode - 1).deepCopy
      branch_array_variable0(m) = save(zbrar(m))
      return branch_array_variable0
    }
  }



  //
  // Unimplemented algebraic operations, need to be implemented in a derived class
  //

  def MIN_SIZE: Int
  def ROOT_INO: Int
  def UBI_READ_RETRIES: Int
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
  def toStr(param0: Int): String
  def <(param0: key, param1: key): Boolean
  def checksum(param0: buffer, param1: Int): Int
  def evict(param0: Int, param1: pcache): pcache
  def is_open(param0: Int, param1: open_files): Boolean
  def pr(param0: Byte, param1: metadata): Boolean
  def pw(param0: Byte, param1: metadata): Boolean
  def px(param0: Byte, param1: metadata): Boolean
  def truncate(param0: Int, param1: Int, param2: pcache): pcache
  def ⊑(param0: path, param1: path): Boolean
}
