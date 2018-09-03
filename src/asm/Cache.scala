// Flashix: a verified file system for flash memory
// (c) 2015-2018 Institute for Software & Systems Engineering <http://isse.de/flashix>
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
      icache.delete(INODE.ino, ERR)
      icache.delete_old(INODE.ino)
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
          fsync_pages(INODE0, INODE.size, ERR)
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

  def fsync_page(INODE0: inode, N: Int, ERR: Ref[error]): Unit = {
    val INO: Int = INODE0.ino
    val PBUF: buffer = new buffer()
    val HIT = Ref[Boolean](helpers.scala.Boolean.uninit)
    val DIRTY = Ref[Boolean](helpers.scala.Boolean.uninit)
    pcache.get(INO, N, PBUF, DIRTY, HIT, ERR)
    if (ERR.get == types.error.ESUCCESS && (HIT.get && DIRTY.get)) {
      afs.writepage(INODE0, N, PBUF, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        afs.check_commit(ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        DIRTY := false
        pcache.set_status(INO, N, DIRTY.get, ERR)
      }
    }
  }

  def fsync_pages(INODE0: inode, SIZE: Int, ERR: Ref[error]): Unit = {
    val ERR0 = Ref[error](types.error.ESUCCESS)
    var N: Int = 0
    while (N * VFS_PAGE_SIZE < SIZE && ERR0.get == types.error.ESUCCESS) {
      fsync_page(INODE0, N, ERR0)
      N = N + 1
    }
    ERR := ERR0.get
  }

  def fsync_trunc(INODE0: inode, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val INO: Int = INODE0.ino
    val HIT = Ref[Boolean](helpers.scala.Boolean.uninit)
    val N = Ref[Int](0)
    tcache.get(INO, N, HIT)
    if (HIT.get) {
      val PAGENO: Int = INODE0.size / VFS_PAGE_SIZE
      val PBUF_OPT = Ref[buffer_opt](types.buffer_opt.none)
      afs.truncate(N.get, PAGENO, PBUF_OPT, INODE0, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        tcache.delete(INO)
        val ERR0 = Ref[error](types.error.uninit)
        icache.set_old(INODE0, ERR0)
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
    val HIT = Ref[Boolean](false)
    val NAME: String = DENT.get.name
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
    val HIT = Ref[Boolean](false)
    val INO: Int = INODE.ino
    val DIRTY = Ref[Boolean](false)
    pcache.get(INO, PAGENO, PBUF, DIRTY, HIT, ERR)
    if (HIT.get && ERR.get == types.error.ESUCCESS) {
      EXISTS := true
    } else {
      val INODE0: inode = types.inode.uninit
      get_flash_inode(INODE, INODE0)
      val TRUNC_SIZE = Ref[Int](0)
      tcache.get(INO, TRUNC_SIZE, HIT)
      var DELETED: Boolean = false
      var SIZE: Int = 0
      if (HIT.get) {
        SIZE = min(INODE0.size, TRUNC_SIZE.get)
        DELETED = SIZE <= PAGENO * VFS_PAGE_SIZE
      } else {
        SIZE = INODE0.size
      }
      if (DELETED) {
        PBUF := zeropage
        EXISTS := false
        ERR := types.error.ESUCCESS
      } else {
        afs.readpage(INODE0, PAGENO, PBUF, EXISTS, ERR)
        if (ERR.get == types.error.ESUCCESS && EXISTS.get) {
          if (HIT.get) {
            var MODIFIED: Boolean = false
            val OFFSET: Int = 0
            val PAD_PAGE: Int = INODE0.size / VFS_PAGE_SIZE
            MODIFIED = PAGENO == PAD_PAGE && (OFFSET != 0 && INODE0.size <= TRUNC_SIZE.get)
            if (MODIFIED) {
              PBUF.fill(zero, OFFSET, VFS_PAGE_SIZE - OFFSET)
            }
            DIRTY := (PAGENO == SIZE / VFS_PAGE_SIZE)
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
    val HIT = Ref[Boolean](helpers.scala.Boolean.uninit)
    val OLD_DIRTY = Ref[Boolean](helpers.scala.Boolean.uninit)
    icache.get_status(INODE.ino, OLD_DIRTY, HIT)
    if (HIT.get && OLD_DIRTY.get != true || HIT.get != true) {
      val ERR = Ref[error](types.error.uninit)
      icache.set_old(INODE, ERR)
    }
  }

  override def sync(ERR: Ref[error]): Unit = {
    afs.sync(ERR)
  }

  override def truncate(N: Int, PAGENO: Int, PBUF_OPT: Ref[buffer_opt], INODE: inode, ERR: Ref[error]): Unit = {
    if (READ_ONLY_MODE) {
      ERR := types.error.EROFS
    } else {
      ERR := types.error.ESUCCESS
      val INO: Int = INODE.ino
      val INODE0: inode = INODE.deepCopy
      val SIZE: Int = INODE.size
      val OFFSET: Int = INODE.size % VFS_PAGE_SIZE
      val MODIFIED: Boolean = SIZE <= N && OFFSET != 0
      val PBUF_OPT0 = Ref[buffer_opt](PBUF_OPT.get.deepCopy)
      val EXISTS = Ref[Boolean](helpers.scala.Boolean.uninit)
      if (MODIFIED) {
        truncate_prepare(INODE, PAGENO, N, PBUF_OPT0, EXISTS, ERR)
      }
      val DIRTY: Boolean = SYNC != true
      if (SYNC && ERR.get == types.error.ESUCCESS) {
        afs.truncate(N, PAGENO, PBUF_OPT0, INODE, ERR)
      }
      if (SYNC != true && (ERR.get == types.error.ESUCCESS && (MODIFIED && EXISTS.get))) {
        val buffer_variable0: buffer = PBUF_OPT0.get.buf.deepCopy
        buffer_variable0.fill(zero, OFFSET, VFS_PAGE_SIZE - OFFSET)
        PBUF_OPT0 := types.buffer_opt.some(buffer_variable0).deepCopy
      }
      if (ERR.get == types.error.ESUCCESS) {
        pcache.truncate(INO, SIZE, N, ERR)
      }
      if (ERR.get == types.error.ESUCCESS && (N < SIZE && DIRTY)) {
        pcache.set_status_nofail(INO, N / VFS_PAGE_SIZE, DIRTY)
      }
      if (ERR.get == types.error.ESUCCESS && (MODIFIED && EXISTS.get)) {
        pcache.set(INO, PAGENO, PBUF_OPT0.get.buf, DIRTY, ERR)
      }
      if (ERR.get == types.error.ESUCCESS && DIRTY) {
        tcache.update(INO, N, ERR)
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
        if (MODIFIED && EXISTS.get) {
          PBUF_OPT := PBUF_OPT0.get
        }
      }
    }
  }

  def truncate_prepare(INODE: inode, PAGENO: Int, N: Int, PBUF_OPT: Ref[buffer_opt], EXISTS: Ref[Boolean], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val HIT = Ref[Boolean](helpers.scala.Boolean.uninit)
    val PBUF: buffer = new buffer(VFS_PAGE_SIZE).fill(0.toByte)
    if (PBUF_OPT.get.isInstanceOf[types.buffer_opt.some]) {
      EXISTS := true
    } else {
      val DIRTY = Ref[Boolean](helpers.scala.Boolean.uninit)
      pcache.get(INODE.ino, PAGENO, PBUF, DIRTY, HIT, ERR)
      if (ERR.get == types.error.ESUCCESS && HIT.get) {
        PBUF_OPT := types.buffer_opt.some(PBUF).deepCopy
        EXISTS := true
      }
    }
    if (PBUF_OPT.get == types.buffer_opt.none && ERR.get == types.error.ESUCCESS) {
      val N0 = Ref[Int](0)
      tcache.get(INODE.ino, N0, HIT)
      val INODE0: inode = types.inode.uninit
      get_flash_inode(INODE, INODE0)
      val EXISTS0 = Ref[Boolean](helpers.scala.Boolean.uninit)
      afs.readpage(INODE0, PAGENO, PBUF, EXISTS0, ERR)
      EXISTS := (EXISTS0.get && ! (HIT.get && min(N0.get, INODE0.size) <= VFS_PAGE_SIZE * PAGENO))
      if (ERR.get == types.error.ESUCCESS && EXISTS.get) {
        PBUF_OPT := types.buffer_opt.some(PBUF).deepCopy
      }
    }
    if (PBUF_OPT.get == types.buffer_opt.none) {
      EXISTS := false
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
