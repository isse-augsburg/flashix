// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package integration

import types._
import types.key._
import encoding.node_header._

final class Algebraic(val mtd: MTDSimulation) extends algebraic.Algebraic {
  private implicit val _algebraic_implicit = this

  //
  // Unimplemented algebraic operations
  //

  override def EB_PAGE_SIZE: Int = mtd.EB_PAGE_SIZE
  override def MIN_SIZE: Int = 4
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
  override def validtrailer: buffer = mkzbuf(NODE_HEADER_SIZE)
  override def zero: Byte = 0

  override def flashsize(param0: group_node_list): Int = param0.list.foldLeft(0){
    case (acc, gnd) =>
      acc + flashsize(gnd)
  }
  override def flashsize(param0: node): Int = flashsize(group_node(param0, 0, false, false))
  override def max(param0: nat_set): Int = param0.set.max
  override def min(param0: nat_set): Int = param0.set.min
  override def toStr(param0: Int): String = param0.toString
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
  override def at(param0: address_list, param1: Int): address = param0(param1)
  override def checksum(param0: buffer, param1: Int): Int = 0
  override def is_open(ino: Int, of: open_files): Boolean = of.map.exists { _._2.ino == ino }
  override def pr(param0: Byte, param1: metadata): Boolean = true
  override def pw(param0: Byte, param1: metadata): Boolean = true
  override def px(param0: Byte, param1: metadata): Boolean = true
  override def to_vtbl(param0: volumes): vtbl = {
    val vtbl = new vtbl()
    param0.map.foreach { case (volumeid, vol) =>
      vtbl += volumeid -> vol.length
    }
    vtbl
  }
  override def ⊑(param0: path, param1: path): Boolean = param0.startsWith(param1)

  // TODO: ab hier sollte es eigentlich Definitionen geben, werden aber manchmal (nicht) mitgeneriert?
  override def below(param0: nat_set, param1: Int): nat_set = {
    new nat_set(param0.set.filter { _ < param1 })
  }
  override def keys(param0: nat_set): key_set = {
    new key_set(param0.set.map { inodekey(_).asInstanceOf[key] })
  }
  override def minus(param0: nat_set, param1: Int): nat_set = {
    new nat_set(param0.set.map { _ - param1 })
  }
  override def isempty(buf: buffer, n: Int, ino: Int): Boolean = {
    if (ino == 0)
      return true
    else
      return buf(n + (ino - 1)) == empty && isempty(buf, n, ino - 1)
  }
  override def rangeeq(buf: buffer, n0: Int, buf0: buffer, n1: Int, ino: Int): Boolean = {
    if (ino == 0)
      return n0 <= buf.length && n1 <= buf0.length
    else
      return buf(n0) == buf0(n1) && rangeeq(buf, n0 + 1, buf0, n1 + 1, ino - 1)
  }

  // Fixed the following implementations
  override def datasize(buf: buffer, m0: Int): Int = {
    // The generated implementation is recursive and leads to a stack overflow
    var cur = m0
    while (cur != 0 && buf(cur - 1) == empty) {
      cur = cur - 1
    }
    return cur
  }
}
