// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error

abstract class afs_interface {
  def afs_check_commit(ERR: Ref[error])
  def afs_create(P_INODE: inode, MD: metadata, DENT: Ref[dentry], ERR: Ref[error])
  def afs_evict(INODE: inode, ERR: Ref[error])
  def afs_format(N: Int, MD: metadata, ERR: Ref[error])
  def afs_iget(INO: Int, INODE: inode, ERR: Ref[error])
  def afs_link(NEW_INO: Int, OLD_DENT: dentry, NEW_DENT: Ref[dentry], ERR: Ref[error])
  def afs_lookup(P_INO: Int, DENT: Ref[dentry], ERR: Ref[error])
  def afs_mkdir(P_INODE: inode, MD: metadata, DENT: Ref[dentry], ERR: Ref[error])
  def afs_readdir(INODE: inode, NAMES: stringset, ERR: Ref[error])
  def afs_readpage(INODE: inode, PAGENO: Int, PBUF: buffer, ERR: Ref[error])
  def afs_recovery(ERR: Ref[error])
  def afs_rename(OLD_INO: Int, NEW_INO: Int, OLD_DENT: Ref[dentry], NEW_DENT: Ref[dentry], ERR: Ref[error])
  def afs_rmdir(P_INODE: inode, DENT: Ref[dentry], ERR: Ref[error])
  def afs_truncate(INODE: inode, N: Int, ERR: Ref[error])
  def afs_unlink(P_INODE: inode, DENT: Ref[dentry], ERR: Ref[error])
  def afs_write_inode(INODE: inode, ERR: Ref[error])
  def afs_writepage(INODE: inode, PAGENO: Int, PBUF: buffer, ERR: Ref[error])
}
