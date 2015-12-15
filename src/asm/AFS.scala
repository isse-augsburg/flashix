// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import types._
import types.error.error

abstract class AFS {
  def afs_check_commit(ERR: Ref[error])
  def afs_create(P_INO: Int, MD: metadata, DENT: Ref[dentry], ERR: Ref[error])
  def afs_evict(INO: Int, ERR: Ref[error])
  def afs_format(N: Int, MD: metadata, ERR: Ref[error])
  def afs_iget(INO: Int, INODE: Ref[inode], ERR: Ref[error])
  def afs_link(NEW_INO: Int, OLD_DENT: dentry, NEW_DENT: Ref[dentry], ERR: Ref[error])
  def afs_lookup(P_INO: Int, DENT: Ref[dentry], ERR: Ref[error])
  def afs_mkdir(P_INO: Int, MD: metadata, DENT: Ref[dentry], ERR: Ref[error])
  def afs_readdir(INO: Int, NAMES: stringset, ERR: Ref[error])
  def afs_readpage(INODE: inode, PAGENO: Int, PBUF: buffer, ERR: Ref[error])
  def afs_rename(OLD_INO: Int, NEW_INO: Int, OLD_DENT: Ref[dentry], NEW_DENT: Ref[dentry], ERR: Ref[error])
  def afs_rmdir(P_INO: Int, DENT: Ref[dentry], ERR: Ref[error])
  def afs_truncate(INODE: inode, N: Int, ERR: Ref[error])
  def afs_unlink(P_INO: Int, DENT: Ref[dentry], ERR: Ref[error])
  def afs_write_inode(INODE: inode, ERR: Ref[error])
  def afs_writepage(INODE: inode, PAGENO: Int, PBUF: buffer, ERR: Ref[error])
}
