// Flashix: a verified file system for flash memory
// (c) 2015-2019 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._
import proc._
import types._
import types.error.error

class Cache(var READ_ONLY_MODE : Boolean, var SYNC : Boolean, val tcache : TcacheInterface, val pcache : PcacheInterface, val icache : IcacheInterface, val dcache : DcacheInterface, val afs : AfsInterface)(implicit _algebraic_implicit: algebraic.Algebraic, _procedures_implicit: proc.Procedures) extends AfsInterface {
  import _algebraic_implicit._
  import _procedures_implicit._

  override def check_commit(ERR: Ref[error]): Unit = {
    afs.check_commit(ERR)
  }

  override def create(MD: metadata, P_INODE: inode, C_INODE: inode, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    if (READ_ONLY_MODE) {
      ERR := types.error.EROFS
    } else {
      val P_INODE0: inode = types.inode.uninit
      get_flash_inode(P_INODE, P_INODE0)
      afs.create(MD, P_INODE0, C_INODE, DENT, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        cache_transfer_inode_changes(P_INODE0, P_INODE)
        icache.set_inode(P_INODE, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          icache.set_inode(C_INODE, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          dcache.set(P_INODE.ino, DENT.get.name, DENT.get, ERR)
        }
      }
    }
  }

  override def evict(INODE: inode, ERR: Ref[error]): Unit = {
    val DIRP: Boolean = INODE.directory
    val INODE0: inode = types.inode.uninit
    get_flash_inode(INODE, INODE0)
    afs.evict(INODE0, ERR)
    if (ERR.get == types.error.ESUCCESS && INODE.nlink == 0) {
      icache.delete_old(INODE.ino)
      icache.delete(INODE.ino, ERR)
      tcache.delete(INODE.ino)
      if (ERR.get == types.error.ESUCCESS && DIRP != true) {
        pcache.evict(INODE.ino, ERR)
      }
    }
  }

  override def format(N: Int, DOSYNC: Boolean, SIZE: Int, MD: metadata, ERR: Ref[error]): Unit = {
    SYNC = DOSYNC
    READ_ONLY_MODE = false
    afs.format(N, DOSYNC, SIZE, MD, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      icache.format(ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      dcache.format(ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      pcache.format(ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      tcache.format(ERR)
    }
  }

  override def fsync(INODE: inode, ISDATASYNC: Boolean, ERR: Ref[error]): Unit = {
    if (READ_ONLY_MODE) {
      ERR := types.error.EROFS
    } else {
      ERR := types.error.ESUCCESS
      val INODE0: inode = types.inode.uninit
      get_flash_inode(INODE, INODE0)
      if (SYNC != true) {
        fsync_trunc(INODE0, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          fsync_pages(INODE0, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          fsync_inode(INODE0, ERR)
        }
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.fsync(INODE0, ISDATASYNC, ERR)
      }
      if (ERR.get != types.error.ESUCCESS) {
        READ_ONLY_MODE = true
      }
    }
  }

  def fsync_inode(INODE0: inode, ERR: Ref[error]): Unit = {
    val INO: Int = INODE0.ino
    val DIRTY = Ref[Boolean](helpers.scala.Boolean.uninit)
    val HIT = Ref[Boolean](helpers.scala.Boolean.uninit)
    val INODE: inode = types.inode.uninit
    icache.get(INO, INODE, DIRTY, HIT, ERR)
    if (ERR.get == types.error.ESUCCESS && (HIT.get && DIRTY.get)) {
      afs.write_meta(INODE0, INODE.meta, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        INODE0.meta = INODE.meta
      }
    }
    if (ERR.get == types.error.ESUCCESS && (HIT.get && (DIRTY.get && (! INODE0.directory && INODE0.size < INODE.size)))) {
      afs.write_size(INODE0, INODE.size, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        INODE0.size = INODE.size
      }
    }
    if (ERR.get == types.error.ESUCCESS) {
      DIRTY := false
      icache.delete_old(INO)
      icache.set_status(INO, DIRTY.get, ERR)
    } else {
      val ERR0 = Ref[error](types.error.uninit)
      icache.set_old(INODE0, ERR0)
    }
  }

  def fsync_page(INODE0: inode, PAGENO: Int, ERR: Ref[error]): Unit = {
    val PBUF: buffer = vfspage
    val HIT = Ref[Boolean](false)
    val INO: Int = INODE0.ino
    val DIRTY = Ref[Boolean](false)
    pcache.get(INO, PAGENO, PBUF, DIRTY, HIT, ERR)
    if (ERR.get == types.error.ESUCCESS && (HIT.get && DIRTY.get)) {
      afs.writepage(INODE0, PAGENO, PBUF, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        afs.check_commit(ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        DIRTY := false
        pcache.set_status(INO, PAGENO, DIRTY.get, ERR)
      }
    }
  }

  def fsync_pages(INODE0: inode, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val MAX_PAGENO = Ref[Int](0)
    pcache.get_max_pageno(INODE0.ino, MAX_PAGENO)
    var PAGENO: Int = 0
    while (PAGENO <= MAX_PAGENO.get && ERR.get == types.error.ESUCCESS) {
      fsync_page(INODE0, PAGENO, ERR)
      PAGENO = PAGENO + 1
    }
  }

  def fsync_trunc(INODE0: inode, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val INO: Int = INODE0.ino
    val HIT = Ref[Boolean](helpers.scala.Boolean.uninit)
    val MINUP_EXISTS = Ref[Boolean](helpers.scala.Boolean.uninit)
    val LAST_TRUNC_SIZE = Ref[Int](0)
    val MIN_TRUNC_SIZE = Ref[Int](0)
    tcache.get(INO, MIN_TRUNC_SIZE, LAST_TRUNC_SIZE, MINUP_EXISTS, HIT)
    if (HIT.get) {
      val PBUF_OPT = Ref[buffer_opt](types.buffer_opt.none)
      afs.truncate(MIN_TRUNC_SIZE.get, INODE0, PBUF_OPT, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val ERR0 = Ref[error](types.error.uninit)
        icache.set_old(INODE0, ERR0)
      }
      if (ERR.get == types.error.ESUCCESS && MINUP_EXISTS.get) {
        afs.truncate(LAST_TRUNC_SIZE.get, INODE0, PBUF_OPT, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          icache.set_old(INODE0, ERR)
        }
      }
      if (ERR.get == types.error.ESUCCESS) {
        tcache.delete(INO)
      }
    }
  }

  override def fsyncdir(INODE: inode, ISDATASYNC: Boolean, ERR: Ref[error]): Unit = {
    if (READ_ONLY_MODE) {
      ERR := types.error.EROFS
    } else {
      afs.fsyncdir(INODE, ISDATASYNC, ERR)
    }
  }

  def get_flash_inode(INODE: inode, INODE0: inode): Unit = {
    val INO: Int = INODE.ino
    INODE0 := INODE.deepCopy
    val HIT = Ref[Boolean](false)
    val SIZE = Ref[Int](0)
    val MD = Ref[metadata](types.metadata.uninit)
    icache.get_old(INO, MD, SIZE, HIT)
    if (HIT.get) {
      INODE0.meta = MD.get
      if (! INODE0.directory) {
        INODE0.size = SIZE.get
      }
    }
  }

  override def iget(INO: Int, INODE: inode, ERR: Ref[error]): Unit = {
    val HIT = Ref[Boolean](false)
    val DIRTY = Ref[Boolean](false)
    icache.get(INO, INODE, DIRTY, HIT, ERR)
    if (HIT.get != true || ERR.get != types.error.ESUCCESS) {
      afs.iget(INO, INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        icache.set(INODE, DIRTY.get, ERR)
      }
    }
  }

  override def link(OLD_DENT: dentry, P_INODE: inode, C_INODE: inode, NEW_DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    if (READ_ONLY_MODE) {
      ERR := types.error.EROFS
    } else {
      val P_INODE0: inode = types.inode.uninit
      get_flash_inode(P_INODE, P_INODE0)
      val C_INODE0: inode = types.inode.uninit
      get_flash_inode(C_INODE, C_INODE0)
      afs.link(OLD_DENT, P_INODE0, C_INODE0, NEW_DENT, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        cache_transfer_inode_changes(P_INODE0, P_INODE)
        cache_transfer_inode_changes(C_INODE0, C_INODE)
        icache.set_inode(P_INODE, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          icache.set_inode(C_INODE, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          dcache.set(P_INODE.ino, NEW_DENT.get.name, NEW_DENT.get, ERR)
        }
      }
    }
  }

  override def lookup(P_INO: Int, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    val NAME: String = DENT.get.name
    val HIT = Ref[Boolean](false)
    dcache.get(P_INO, NAME, DENT, HIT, ERR)
    if (HIT.get != true || ERR.get != types.error.ESUCCESS) {
      afs.lookup(P_INO, DENT, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        dcache.set(P_INO, NAME, DENT.get, ERR)
      }
    }
  }

  override def mkdir(MD: metadata, P_INODE: inode, C_INODE: inode, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    if (READ_ONLY_MODE) {
      ERR := types.error.EROFS
    } else {
      val P_INODE0: inode = types.inode.uninit
      get_flash_inode(P_INODE, P_INODE0)
      afs.mkdir(MD, P_INODE0, C_INODE, DENT, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        cache_transfer_inode_changes(P_INODE0, P_INODE)
        icache.set_inode(P_INODE, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          icache.set_inode(C_INODE, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          dcache.set(P_INODE.ino, DENT.get.name, DENT.get, ERR)
        }
      }
    }
  }

  override def readdir(INODE: inode, NAMES: stringset, ERR: Ref[error]): Unit = {
    afs.readdir(INODE, NAMES, ERR)
  }

  override def readpage(INODE: inode, PAGENO: Int, PBUF: buffer, EXISTS: Ref[Boolean], ERR: Ref[error]): Unit = {
    val INO: Int = INODE.ino
    val HIT = Ref[Boolean](false)
    val DIRTY = Ref[Boolean](false)
    pcache.get(INO, PAGENO, PBUF, DIRTY, HIT, ERR)
    if (HIT.get && ERR.get == types.error.ESUCCESS) {
      EXISTS := true
    } else {
      val INODE0: inode = types.inode.uninit
      get_flash_inode(INODE, INODE0)
      val MINUP_EXISTS = Ref[Boolean](false)
      val MIN_TRUNC_SIZE = Ref[Int](0)
      val LAST_TRUNC_SIZE = Ref[Int](0)
      tcache.get(INO, MIN_TRUNC_SIZE, LAST_TRUNC_SIZE, MINUP_EXISTS, HIT)
      var SIZE: Int = 0
      var DELETED: Boolean = false
      if (HIT.get) {
        SIZE = min(INODE0.size, MIN_TRUNC_SIZE.get)
        val POSITION = Ref[Int](0)
        vfs_pos(PAGENO, POSITION)
        DELETED = SIZE <= POSITION.get
      } else {
        SIZE = INODE0.size
      }
      if (DELETED) {
        PBUF := zeropage
        ERR := types.error.ESUCCESS
        EXISTS := false
      } else {
        afs.readpage(INODE0, PAGENO, PBUF, EXISTS, ERR)
        if (ERR.get == types.error.ESUCCESS && EXISTS.get) {
          if (HIT.get && (MINUP_EXISTS.get || INODE0.size <= MIN_TRUNC_SIZE.get)) {
            val PAD_PAGE = Ref[Int](0)
            vfs_pageno(INODE0.size, PAD_PAGE)
            if (PAGENO == PAD_PAGE.get && INODE0.size <= MIN_TRUNC_SIZE.get) {
              val IS_MODIFIED = Ref[Boolean](false)
              vfs_page_truncate(INODE0.size, PBUF, IS_MODIFIED)
            }
            val ACTUAL_PAGENO = Ref[Int](0)
            vfs_pageno(SIZE, ACTUAL_PAGENO)
            DIRTY := (PAGENO == ACTUAL_PAGENO.get)
          } else {
            DIRTY := false
          }
          pcache.set(INO, PAGENO, PBUF, DIRTY.get, ERR)
        }
      }
    }
  }

  override def recovery(DOSYNC: Boolean, ERR: Ref[error]): Unit = {
    SYNC = DOSYNC
    READ_ONLY_MODE = false
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
    if (ERR.get == types.error.ESUCCESS) {
      tcache.recovery(ERR)
    }
  }

  override def rename(OLD_CHILD_INODE: inode, OLD_PARENT_INODE: inode, NEW_PARENT_INODE: inode, NEW_CHILD_INODE: inode, OLD_DENT: Ref[dentry], NEW_DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    if (READ_ONLY_MODE) {
      ERR := types.error.EROFS
    } else {
      val OLD_PARENT_INODE0: inode = types.inode.uninit
      get_flash_inode(OLD_PARENT_INODE, OLD_PARENT_INODE0)
      val NEW_PARENT_INODE0: inode = types.inode.uninit
      get_flash_inode(NEW_PARENT_INODE, NEW_PARENT_INODE0)
      val DENTP: Boolean = NEW_DENT.get.isInstanceOf[types.dentry.mkdentry]
      val NEW_CHILD_INODE0: inode = types.inode.uninit
      val OLD_CHILD_INODE0: inode = types.inode.uninit
      get_flash_inode(OLD_CHILD_INODE, OLD_CHILD_INODE0)
      if (DENTP) {
        get_flash_inode(NEW_CHILD_INODE, NEW_CHILD_INODE0)
      }
      afs.rename(OLD_CHILD_INODE0, OLD_PARENT_INODE0, NEW_PARENT_INODE0, NEW_CHILD_INODE0, OLD_DENT, NEW_DENT, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        cache_transfer_inode_changes(OLD_PARENT_INODE0, OLD_PARENT_INODE)
        cache_transfer_inode_changes(NEW_PARENT_INODE0, NEW_PARENT_INODE)
        icache.set_inode(OLD_PARENT_INODE, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          icache.set_inode(NEW_PARENT_INODE, ERR)
        }
        if (ERR.get == types.error.ESUCCESS && DENTP) {
          cache_transfer_inode_changes(NEW_CHILD_INODE0, NEW_CHILD_INODE)
          icache.set_inode(NEW_CHILD_INODE, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          dcache.delete(OLD_PARENT_INODE.ino, OLD_DENT.get.name, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          dcache.set(NEW_PARENT_INODE.ino, NEW_DENT.get.name, NEW_DENT.get, ERR)
        }
      }
    }
  }

  override def rmdir(P_INODE: inode, C_INODE: inode, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    if (READ_ONLY_MODE) {
      ERR := types.error.EROFS
    } else {
      val P_INODE0: inode = types.inode.uninit
      get_flash_inode(P_INODE, P_INODE0)
      val C_INODE0: inode = types.inode.uninit
      get_flash_inode(C_INODE, C_INODE0)
      icache.exchange_old(P_INODE0, ERR)
      afs.rmdir(P_INODE0, C_INODE0, DENT, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        cache_transfer_inode_changes(P_INODE0, P_INODE)
        cache_transfer_inode_changes(C_INODE0, C_INODE)
        icache.set_inode(C_INODE, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          icache.set_inode(P_INODE, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          dcache.delete(P_INODE.ino, DENT.get.name, ERR)
        }
      }
    }
  }

  def set_flash_inode(INODE: inode): Unit = {
    val OLD_DIRTY = Ref[Boolean](false)
    val HIT = Ref[Boolean](false)
    icache.get_status(INODE.ino, OLD_DIRTY, HIT)
    if (HIT.get && OLD_DIRTY.get != true || HIT.get != true) {
      val ERR = Ref[error](types.error.ESUCCESS)
      icache.set_old(INODE, ERR)
    }
  }

  override def sync(ERR: Ref[error]): Unit = {
    afs.sync(ERR)
  }

  override def truncate(N: Int, INODE: inode, PBUF_OPT: Ref[buffer_opt], ERR: Ref[error]): Unit = {
    if (READ_ONLY_MODE) {
      ERR := types.error.EROFS
      PBUF_OPT := types.buffer_opt.none
    } else {
      PBUF_OPT := types.buffer_opt.none
      ERR := types.error.ESUCCESS
      val SIZE: Int = INODE.size
      val PAGENO = Ref[Int](0)
      vfs_pageno(SIZE, PAGENO)
      val INO: Int = INODE.ino
      val INODE0: inode = INODE.deepCopy
      val DIRTY: Boolean = SYNC != true
      if (SYNC) {
        afs.truncate(N, INODE, PBUF_OPT, ERR)
      } else       if (SIZE <= N) {
        truncate_prepare(INODE, PAGENO.get, PBUF_OPT, ERR)
        if (ERR.get == types.error.ESUCCESS && PBUF_OPT.get.isInstanceOf[types.buffer_opt.some]) {
          val PBUF: buffer = PBUF_OPT.get.buf.deepCopy
          val IS_MODIFIED = Ref[Boolean](false)
          vfs_page_truncate(SIZE, PBUF, IS_MODIFIED)
          if (IS_MODIFIED.get) {
            PBUF_OPT := types.buffer_opt.some(PBUF).deepCopy
          } else {
            PBUF_OPT := types.buffer_opt.none
          }
        }
      }
      if (ERR.get == types.error.ESUCCESS) {
        pcache.truncate(INO, SIZE, N, ERR)
      }
      if (ERR.get == types.error.ESUCCESS && PBUF_OPT.get.isInstanceOf[types.buffer_opt.some]) {
        pcache.set(INO, PAGENO.get, PBUF_OPT.get.buf, DIRTY, ERR)
      }
      if (ERR.get == types.error.ESUCCESS && DIRTY) {
        tcache.update(INO, N, SIZE, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        if (DIRTY) {
          set_flash_inode(INODE0)
        }
        INODE0.size = N
        icache.set(INODE0, DIRTY, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        INODE := INODE0
      }
    }
  }

  def truncate_prepare(INODE: inode, PAGENO: Int, PBUF_OPT: Ref[buffer_opt], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val HIT = Ref[Boolean](false)
    val PBUF: buffer = vfspage
    val DIRTY = Ref[Boolean](false)
    pcache.get(INODE.ino, PAGENO, PBUF, DIRTY, HIT, ERR)
    if (ERR.get == types.error.ESUCCESS && HIT.get) {
      PBUF_OPT := types.buffer_opt.some(PBUF).deepCopy
    } else {
      PBUF_OPT := types.buffer_opt.none
    }
    if (PBUF_OPT.get == types.buffer_opt.none && ERR.get == types.error.ESUCCESS) {
      val INODE0: inode = INODE.deepCopy
      val MIN_TRUNC_SIZE = Ref[Int](0)
      val LAST_TRUNC_SIZE = Ref[Int](0)
      val MINUP_EXISTS = Ref[Boolean](false)
      tcache.get(INODE.ino, MIN_TRUNC_SIZE, LAST_TRUNC_SIZE, MINUP_EXISTS, HIT)
      get_flash_inode(INODE, INODE0)
      if (HIT.get != true || VFS_PAGE_SIZE * PAGENO < min(MIN_TRUNC_SIZE.get, INODE0.size)) {
        val EXISTS = Ref[Boolean](false)
        afs.readpage(INODE0, PAGENO, PBUF, EXISTS, ERR)
        if (ERR.get == types.error.ESUCCESS && EXISTS.get) {
          PBUF_OPT := types.buffer_opt.some(PBUF).deepCopy
        }
      }
    }
  }

  override def unlink(P_INODE: inode, C_INODE: inode, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    if (READ_ONLY_MODE) {
      ERR := types.error.EROFS
    } else {
      val P_INODE0: inode = types.inode.uninit
      get_flash_inode(P_INODE, P_INODE0)
      val C_INODE0: inode = types.inode.uninit
      get_flash_inode(C_INODE, C_INODE0)
      afs.unlink(P_INODE0, C_INODE0, DENT, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        cache_transfer_inode_changes(P_INODE0, P_INODE)
        cache_transfer_inode_changes(C_INODE0, C_INODE)
        icache.set_inode(C_INODE, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          icache.set_inode(P_INODE, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          dcache.delete(P_INODE.ino, DENT.get.name, ERR)
        }
      }
    }
  }

  override def write_begin(INODE: inode, ERR: Ref[error]): Unit = {
    if (READ_ONLY_MODE) {
      ERR := types.error.EROFS
    } else     if (SYNC) {
      afs.write_begin(INODE, ERR)
    } else {
      ERR := types.error.ESUCCESS
      val SIZE: Int = INODE.size
      val INO: Int = INODE.ino
      pcache.write_begin(INO, SIZE)
      tcache.write_begin(INO, SIZE)
      val ALIGNED = Ref[Boolean](false)
      vfs_aligned(SIZE, ALIGNED)
      if (ALIGNED.get != true) {
        val PAGENO = Ref[Int](0)
        vfs_pageno(SIZE, PAGENO)
        val PBUF_OPT = Ref[buffer_opt](types.buffer_opt.none)
        truncate_prepare(INODE, PAGENO.get, PBUF_OPT, ERR)
        if (ERR.get == types.error.ESUCCESS && PBUF_OPT.get.isInstanceOf[types.buffer_opt.some]) {
          val PBUF: buffer = PBUF_OPT.get.buf.deepCopy
          val IS_MODIFIED = Ref[Boolean](false)
          vfs_page_truncate(SIZE, PBUF, IS_MODIFIED)
          pcache.set(INO, PAGENO.get, PBUF, true, ERR)
        }
      }
    }
  }

  override def write_meta(INODE: inode, MD: metadata, ERR: Ref[error]): Unit = {
    if (READ_ONLY_MODE) {
      ERR := types.error.EROFS
    } else {
      ERR := types.error.ESUCCESS
      var DIRTY: Boolean = true
      if (SYNC || INODE.directory) {
        afs.write_meta(INODE, MD, ERR)
        DIRTY = false
      }
      if (ERR.get == types.error.ESUCCESS && DIRTY) {
        set_flash_inode(INODE)
      }
      if (ERR.get == types.error.ESUCCESS) {
        val NEW_INODE: inode = INODE.copy(meta = MD).deepCopy
        icache.set(NEW_INODE, DIRTY, ERR)
      }
    }
  }

  override def write_size(INODE: inode, SIZE: Int, ERR: Ref[error]): Unit = {
    if (READ_ONLY_MODE) {
      ERR := types.error.EROFS
    } else {
      ERR := types.error.ESUCCESS
      var DIRTY: Boolean = true
      if (SYNC) {
        afs.write_size(INODE, SIZE, ERR)
        DIRTY = false
      }
      if (ERR.get == types.error.ESUCCESS && DIRTY) {
        set_flash_inode(INODE)
      }
      if (ERR.get == types.error.ESUCCESS) {
        val NEW_INODE: inode = INODE.copy(size = SIZE).deepCopy
        icache.set(NEW_INODE, DIRTY, ERR)
      }
    }
  }

  override def writepage(INODE: inode, PAGENO: Int, PBUF: buffer, ERR: Ref[error]): Unit = {
    if (READ_ONLY_MODE) {
      ERR := types.error.EROFS
    } else {
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

}
