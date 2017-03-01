// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error

class cache_asm(var SYNC : Boolean, val afs : afs_interface, val icache : icache_interface, val dcache : dcache_interface, val pcache : pcache_interface)(implicit _algebraic_implicit: algebraic.Algebraic) extends afs_interface {
  import _algebraic_implicit._

  override def check_commit(ERR: Ref[error]): Unit = {
    afs.check_commit(ERR)
  }

  override def create(MD: metadata, P_INODE: inode, C_INODE: inode, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    afs.create(MD, P_INODE, C_INODE, DENT, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val DIRTY: Boolean = false
      icache.set(P_INODE, DIRTY, ERR)
      icache.set(C_INODE, DIRTY, ERR)
      dcache.set(P_INODE.ino, DENT.get.name, DENT.get, ERR)
    }
  }

  override def evict(INODE: inode, ERR: Ref[error]): Unit = {
    val DIRP: Boolean = INODE.directory
    afs.evict(INODE, ERR)
    if (ERR.get == types.error.ESUCCESS && INODE.nlink == 0) {
      icache.delete(INODE.ino, ERR)
      if (DIRP != true) {
        pcache.evict(INODE.ino, ERR)
      }
    }
  }

  override def format(N: Int, DOSYNC: Boolean, MD: metadata, ERR: Ref[error]): Unit = {
    SYNC = DOSYNC
    afs.format(N, DOSYNC, MD, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      icache.format(ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      dcache.format(ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      pcache.format(ERR)
    }
  }

  override def fsync(P_INODE: inode, ISDATASYNC: Boolean, ERR: Ref[error]): Unit = {
    val INODE: inode = P_INODE.deepCopy
    ERR := types.error.ESUCCESS
    if (SYNC != true) {
      fsync_pages(INODE, ERR)
      if (ERR.get == types.error.ESUCCESS && ISDATASYNC != true) {
        fsync_inode(INODE, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.fsync(INODE, ISDATASYNC, ERR)
      }
    }
  }

  def fsync_inode(INODE: inode, ERR: Ref[error]): Unit = {
    val INO: Int = INODE.ino
    val DIRTY = Ref[Boolean](helpers.scala.Boolean.uninit)
    val HIT = Ref[Boolean](helpers.scala.Boolean.uninit)
    icache.check_inode(INO, HIT, DIRTY, ERR)
    if (ERR.get == types.error.ESUCCESS && (HIT.get && DIRTY.get)) {
      icache.get(INO, HIT, INODE, ERR)
    }
    if (ERR.get == types.error.ESUCCESS && (HIT.get && DIRTY.get)) {
      afs.write_inode(INODE, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      DIRTY := false
      icache.set_status(INO, DIRTY.get, ERR)
    }
  }

  def fsync_pages(INODE: inode, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    var N: Int = 0
    val INO: Int = INODE.ino
    val DIRTY = Ref[Boolean](helpers.scala.Boolean.uninit)
    val HIT = Ref[Boolean](helpers.scala.Boolean.uninit)
    while (N * VFS_PAGE_SIZE < INODE.size && ERR.get == types.error.ESUCCESS) {
      pcache.check_page(INO, N, HIT, DIRTY, ERR)
      if (HIT.get && DIRTY.get) {
        val PBUF: buffer = new buffer()
        pcache.get(INO, N, HIT, PBUF, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          afs.writepage(INODE, N, PBUF, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          afs.check_commit(ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          DIRTY := false
          pcache.set_status(INO, N, DIRTY.get, ERR)
        }
      }
      N = N + 1
    }
  }

  override def fsyncdir(INODE: inode, ISDATASYNC: Boolean, ERR: Ref[error]): Unit = {
    afs.fsyncdir(INODE, ISDATASYNC, ERR)
  }

  override def iget(INO: Int, INODE: inode, ERR: Ref[error]): Unit = {
    val HIT = Ref[Boolean](false)
    icache.get(INO, HIT, INODE, ERR)
    if (HIT.get != true || ERR.get != types.error.ESUCCESS) {
      afs.iget(INO, INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val DIRTY: Boolean = false
        icache.set(INODE, DIRTY, ERR)
      }
    }
  }

  override def link(OLD_DENT: dentry, P_INODE: inode, C_INODE: inode, NEW_DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    afs.link(OLD_DENT, P_INODE, C_INODE, NEW_DENT, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val DIRTY: Boolean = false
      icache.set(P_INODE, DIRTY, ERR)
      icache.set(C_INODE, DIRTY, ERR)
      dcache.set(P_INODE.ino, NEW_DENT.get.name, NEW_DENT.get, ERR)
    }
  }

  override def lookup(P_INO: Int, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    val HIT = Ref[Boolean](false)
    val NAME: String = DENT.get.name
    dcache.get(P_INO, NAME, HIT, DENT, ERR)
    if (HIT.get != true || ERR.get != types.error.ESUCCESS) {
      afs.lookup(P_INO, DENT, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        dcache.set(P_INO, NAME, DENT.get, ERR)
      }
    }
  }

  override def mkdir(MD: metadata, P_INODE: inode, C_INODE: inode, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    afs.mkdir(MD, P_INODE, C_INODE, DENT, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val DIRTY: Boolean = false
      icache.set(P_INODE, DIRTY, ERR)
      icache.set(C_INODE, DIRTY, ERR)
      dcache.set(P_INODE.ino, DENT.get.name, DENT.get, ERR)
    }
  }

  override def readdir(INODE: inode, NAMES: stringset, ERR: Ref[error]): Unit = {
    afs.readdir(INODE, NAMES, ERR)
  }

  override def readpage(INODE: inode, PAGENO: Int, PBUF: buffer, ERR: Ref[error]): Unit = {
    val HIT = Ref[Boolean](false)
    val INO: Int = INODE.ino
    pcache.get(INO, PAGENO, HIT, PBUF, ERR)
    if (HIT.get != true || ERR.get != types.error.ESUCCESS) {
      afs.readpage(INODE, PAGENO, PBUF, ERR)
      if (ERR.get == types.error.ESUCCESS && PBUF != zeropage) {
        val DIRTY: Boolean = false
        pcache.set(INO, PAGENO, PBUF, DIRTY, ERR)
      }
    }
  }

  override def recovery(DOSYNC: Boolean, ERR: Ref[error]): Unit = {
    SYNC = DOSYNC
    afs.recovery(DOSYNC, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      icache.recovery(ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      dcache.recovery(ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      pcache.recovery(ERR)
    }
  }

  override def rename(OLD_PARENT_INODE: inode, NEW_PARENT_INODE: inode, OLD_CHILD_INODE: inode, NEW_CHILD_INODE: inode, OLD_DENT: Ref[dentry], NEW_DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    val DENTP: Boolean = NEW_DENT.get.isInstanceOf[types.dentry.mkdentry]
    afs.rename(OLD_PARENT_INODE, NEW_PARENT_INODE, OLD_CHILD_INODE, NEW_CHILD_INODE, OLD_DENT, NEW_DENT, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val DIRTY: Boolean = false
      icache.set(OLD_PARENT_INODE, DIRTY, ERR)
      icache.set(NEW_PARENT_INODE, DIRTY, ERR)
      if (DENTP) {
        icache.set(NEW_CHILD_INODE, DIRTY, ERR)
      }
      dcache.delete(OLD_PARENT_INODE.ino, OLD_DENT.get.name, ERR)
      dcache.set(NEW_PARENT_INODE.ino, NEW_DENT.get.name, NEW_DENT.get, ERR)
    }
  }

  override def rmdir(P_INODE: inode, C_INODE: inode, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    afs.rmdir(P_INODE, C_INODE, DENT, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val DIRTY: Boolean = false
      icache.set(C_INODE, DIRTY, ERR)
      icache.set(P_INODE, DIRTY, ERR)
      dcache.delete(P_INODE.ino, DENT.get.name, ERR)
    }
  }

  override def sync(ERR: Ref[error]): Unit = {
    afs.sync(ERR)
  }

  override def truncate(N: Int, PAGENO: Int, PBUF_OPT: Ref[buffer_opt], INODE: inode, MODIFIED: Ref[Boolean], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val SOME_BUF: Boolean = PBUF_OPT.get.isInstanceOf[types.buffer_opt.some]
    val INO: Int = INODE.ino
    if (PBUF_OPT.get == types.buffer_opt.none) {
      val HIT = Ref[Boolean](false)
      val OLD_DIRTY = Ref[Boolean](false)
      pcache.check_page(INO, PAGENO, HIT, OLD_DIRTY, ERR)
      val PBUF: buffer = new buffer()
      if (ERR.get == types.error.ESUCCESS && HIT.get) {
        pcache.get(INO, PAGENO, HIT, PBUF, ERR)
      }
      if (ERR.get == types.error.ESUCCESS && HIT.get) {
        PBUF_OPT := types.buffer_opt.some(PBUF).deepCopy
      }
    }
    val SIZE: Int = INODE.size
    if (ERR.get == types.error.ESUCCESS) {
      afs.truncate(N, PAGENO, PBUF_OPT, INODE, MODIFIED, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      val DIRTY: Boolean = false
      icache.set(INODE, DIRTY, ERR)
      pcache.truncate(INODE.ino, SIZE, N, ERR)
      if (MODIFIED.get) {
        pcache.set(INODE.ino, PAGENO, PBUF_OPT.get.buf, DIRTY, ERR)
      }
    }
    if ((MODIFIED.get != true || ERR.get != types.error.ESUCCESS) && SOME_BUF != true) {
      PBUF_OPT := types.buffer_opt.none
    }
  }

  override def unlink(P_INODE: inode, C_INODE: inode, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    afs.unlink(P_INODE, C_INODE, DENT, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val DIRTY: Boolean = false
      icache.set(C_INODE, DIRTY, ERR)
      icache.set(P_INODE, DIRTY, ERR)
      dcache.delete(P_INODE.ino, DENT.get.name, ERR)
    }
  }

  override def write_inode(INODE: inode, ERR: Ref[error]): Unit = {
    var DIRTY: Boolean = true
    if (SYNC || INODE.directory) {
      afs.write_inode(INODE, ERR)
      DIRTY = false
    } else {
      ERR := types.error.ESUCCESS
    }
    val HIT = Ref[Boolean](false)
    val INODE0: inode = types.inode.uninit
    if (ERR.get == types.error.ESUCCESS) {
      icache.get(INODE.ino, HIT, INODE0, ERR)
    }
    if (ERR.get == types.error.ESUCCESS && HIT.get) {
      INODE0.meta = INODE.meta
      if (! INODE.directory) {
        INODE0.size = INODE.size
      }
      icache.set(INODE0, DIRTY, ERR)
    } else     if (DIRTY) {
      ERR := types.error.EFAIL
    }
  }

  override def writepage(INODE: inode, PAGENO: Int, PBUF: buffer, ERR: Ref[error]): Unit = {
    var DIRTY: Boolean = true
    if (SYNC) {
      afs.writepage(INODE, PAGENO, PBUF, ERR)
      DIRTY = false
    } else {
      ERR := types.error.ESUCCESS
    }
    if (ERR.get == types.error.ESUCCESS) {
      pcache.set(INODE.ino, PAGENO, PBUF, DIRTY, ERR)
    }
  }

}

