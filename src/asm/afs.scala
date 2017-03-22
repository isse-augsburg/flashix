// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error

abstract class afs_interface {
  def check_commit(ERR: Ref[error])
  def create(MD: metadata, P_INODE: inode, C_INODE: inode, DENT: Ref[dentry], ERR: Ref[error])
  def evict(INODE: inode, ERR: Ref[error])
  def format(N: Int, DOSYNC: Boolean, SIZE: Int, MD: metadata, ERR: Ref[error])
  def fsync(INODE: inode, ISDATASYNC: Boolean, ERR: Ref[error])
  def fsyncdir(INODE: inode, ISDATASYNC: Boolean, ERR: Ref[error])
  def iget(INO: Int, INODE: inode, ERR: Ref[error])
  def link(OLD_DENT: dentry, P_INODE: inode, C_INODE: inode, NEW_DENT: Ref[dentry], ERR: Ref[error])
  def lookup(P_INO: Int, DENT: Ref[dentry], ERR: Ref[error])
  def mkdir(MD: metadata, P_INODE: inode, C_INODE: inode, DENT: Ref[dentry], ERR: Ref[error])
  def readdir(INODE: inode, NAMES: stringset, ERR: Ref[error])
  def readpage(INODE: inode, PAGENO: Int, PBUF: buffer, ERR: Ref[error])
  def recovery(DOSYNC: Boolean, ERR: Ref[error])
  def rename(OLD_PARENT_INODE: inode, NEW_PARENT_INODE: inode, OLD_CHILD_INODE: inode, NEW_CHILD_INODE: inode, OLD_DENT: Ref[dentry], NEW_DENT: Ref[dentry], ERR: Ref[error])
  def rmdir(P_INODE: inode, C_INODE: inode, DENT: Ref[dentry], ERR: Ref[error])
  def sync(ERR: Ref[error])
  def truncate(N: Int, PAGENO: Int, PBUF_OPT: Ref[buffer_opt], INODE: inode, MODIFIED: Ref[Boolean], ERR: Ref[error])
  def unlink(P_INODE: inode, C_INODE: inode, DENT: Ref[dentry], ERR: Ref[error])
  def write_inode(INODE: inode, ERR: Ref[error])
  def writepage(INODE: inode, PAGENO: Int, PBUF: buffer, ERR: Ref[error])
}
