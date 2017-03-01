// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import sorts._
import types._
import types.error.error
import types.file_mode.file_mode
import types.seekflag.seekflag

class vfs_asm(var MAXINO : Int, val OF : open_files, val afs : afs_interface)(implicit _algebraic_implicit: algebraic.Algebraic) extends posix_interface {
  import _algebraic_implicit._

  override def close(FD: Int, USER: user, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    if (! OF.contains(FD)) {
      ERR := types.error.EBADFD
    } else {
      val DEL_INO: Int = OF(FD).ino
      val DEL_INODE: inode = types.inode.uninit
      afs.iget(DEL_INO, DEL_INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        OF -= FD
        put_inode(DEL_INODE, ERR)
      }
    }
  }

  override def create(PATH: path, MD: metadata, USER: user, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty) {
      ERR := types.error.EEXISTS
    } else {
      val INO = Ref[Int](ROOT_INO)
      val DENT = Ref[dentry](types.dentry.negdentry(PATH.last))
      val p: path = PATH.init
      walk(p, USER, INO, ERR)
      val INODE: inode = types.inode.uninit
      if (ERR.get == types.error.ESUCCESS) {
        may_create(INO.get, DENT.get, USER, INODE, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.check_commit(ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        val C_INODE: inode = types.inode.uninit
        afs.create(MD, INODE, C_INODE, DENT, ERR)
      }
    }
  }

  override def format(N: Int, DOSYNC: Boolean, MD: metadata, ERR: Ref[error]): Unit = {
    OF.clear
    MAXINO = 0
    afs.format(N, DOSYNC, MD, ERR)
  }

  override def fsync(PATH: path, ISDATASYNC: Boolean, USER: user, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val INO = Ref[Int](ROOT_INO)
    walk(PATH, USER, INO, ERR)
    val INODE: inode = types.inode.uninit
    if (ERR.get == types.error.ESUCCESS) {
      val MODE: file_mode = types.file_mode.MODE_R
      val ISDIR: Boolean = false
      may_open(INO.get, ISDIR, MODE, USER, INODE, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      afs.fsync(INODE, ISDATASYNC, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      afs.sync(ERR)
    }
  }

  override def fsyncdir(PATH: path, ISDATASYNC: Boolean, USER: user, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val INO = Ref[Int](ROOT_INO)
    walk(PATH, USER, INO, ERR)
    val INODE: inode = types.inode.uninit
    if (ERR.get == types.error.ESUCCESS) {
      val MODE: file_mode = types.file_mode.MODE_R
      val ISDIR: Boolean = true
      may_open(INO.get, ISDIR, MODE, USER, INODE, ERR)
    }
    val NAMES: stringset = new stringset()
    if (ERR.get == types.error.ESUCCESS) {
      afs.readdir(INODE, NAMES, ERR)
    }
    val C_INODE: inode = types.inode.uninit
    val DENT = Ref[dentry](types.dentry.uninit)
    while (! NAMES.isEmpty && ERR.get == types.error.ESUCCESS) {
      val NAME: String = NAMES.head
      DENT := types.dentry.negdentry(NAME)
      afs.lookup(INODE.ino, DENT, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        afs.iget(DENT.get.ino, C_INODE, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        if (C_INODE.directory) {
          val PATH_ : path = PATH :+ NAME
          fsyncdir(PATH_, ISDATASYNC, USER, ERR)
        } else {
          afs.fsync(C_INODE, ISDATASYNC, ERR)
        }
      }
      NAMES -= NAME
    }
    if (ERR.get == types.error.ESUCCESS) {
      afs.sync(ERR)
    }
  }

  override def link(PATH: path, PATH_ : path, USER: user, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty || PATH_.isEmpty) {
      ERR := types.error.EISDIR
    } else {
      var OLD_DENT: dentry = types.dentry.uninit
      val INODE: inode = types.inode.uninit
      val C_INODE: inode = types.inode.uninit
      val DENT0 = Ref[dentry](types.dentry.negdentry(PATH.last))
      val N = Ref[Int](ROOT_INO)
      val PATH1: path = PATH.init
      walk(PATH1, USER, N, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        may_link(N.get, USER, INODE, DENT0, ERR)
      }
      C_INODE := INODE.deepCopy
      OLD_DENT = DENT0.get
      val NEW_DENT = Ref[dentry](types.dentry.uninit)
      val NEW_INODE: inode = types.inode.uninit
      if (ERR.get == types.error.ESUCCESS) {
        val DENT: dentry = types.dentry.negdentry(PATH_.last)
        val INO = Ref[Int](ROOT_INO)
        val PATH: path = PATH_.init
        walk(PATH, USER, INO, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          may_create(INO.get, DENT, USER, INODE, ERR)
        }
        NEW_INODE := INODE
        NEW_DENT := DENT
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.check_commit(ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.link(OLD_DENT, NEW_INODE, C_INODE, NEW_DENT, ERR)
      }
    }
  }

  def may_create(INO: Int, DENT0: dentry, USER: user, INODE: inode, ERR: Ref[error]): Unit = {
    val DENT = Ref[dentry](DENT0)
    afs.iget(INO, INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (! INODE.directory) {
        ERR := types.error.ENOTDIR
      } else       if (! px(USER, INODE.meta) || ! pw(USER, INODE.meta)) {
        ERR := types.error.EACCESS
      } else {
        afs.lookup(INO, DENT, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          ERR := types.error.EEXISTS
        } else         if (ERR.get == types.error.ENOENT) {
          ERR := types.error.ESUCCESS
        }
      }
    }
  }

  def may_delete(INO: Int, ISRENAME: Boolean, USER: user, INODE: inode, DENT: Ref[dentry], DENT_INODE: inode, ISDIR: Ref[Boolean], ERR: Ref[error]): Unit = {
    afs.iget(INO, INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (! INODE.directory) {
        ERR := types.error.ENOTDIR
      } else       if (! px(USER, INODE.meta) || ! pw(USER, INODE.meta)) {
        ERR := types.error.EACCESS
      } else {
        afs.lookup(INO, DENT, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          afs.iget(DENT.get.ino, DENT_INODE, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            ISDIR := DENT_INODE.directory
            if (ISDIR.get) {
              if (DENT_INODE.ino == ROOT_INO) {
                ERR := types.error.EACCESS
              } else               if (ISRENAME && ! pw(USER, DENT_INODE.meta)) {
                ERR := types.error.EACCESS
              } else               if (ISRENAME != true && DENT_INODE.size != 0) {
                ERR := types.error.ENOTEMPTY
              }
            }
          }
        }
      }
    }
  }

  def may_delete_check(INO: Int, USER: user, INODE: inode, ISDIR: Ref[Boolean], DENT: Ref[dentry], DENT_INODE: inode, ERR: Ref[error]): Unit = {
    val ISDIR0: Boolean = ISDIR.get
    val ISRENAME: Boolean = false
    may_delete(INO, ISRENAME, USER, INODE, DENT, DENT_INODE, ISDIR, ERR)
    if (ISDIR0 && ISDIR.get != true) {
      ERR := types.error.ENOTDIR
    }
    if (ISDIR0 != true && ISDIR.get) {
      ERR := types.error.EISDIR
    }
  }

  def may_link(INO: Int, USER: user, INODE: inode, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    afs.iget(INO, INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (! INODE.directory) {
        ERR := types.error.ENOTDIR
      } else       if (! px(USER, INODE.meta)) {
        ERR := types.error.EACCESS
      } else {
        afs.lookup(INO, DENT, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          afs.iget(DENT.get.ino, INODE, ERR)
        }
        if (ERR.get == types.error.ESUCCESS && INODE.directory) {
          ERR := types.error.EISDIR
        }
      }
    }
  }

  def may_lookup(INO: Int, USER: user, INODE: inode, ERR: Ref[error]): Unit = {
    afs.iget(INO, INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (! INODE.directory) {
        ERR := types.error.ENOENT
      } else       if (! px(USER, INODE.meta)) {
        ERR := types.error.EACCESS
      }
    }
  }

  def may_open(INO: Int, ISDIR: Boolean, MODE: file_mode, USER: user, INODE: inode, ERR: Ref[error]): Unit = {
    afs.iget(INO, INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (! (INODE.directory == ISDIR)) {
        ERR := (if (INODE.directory) types.error.EISDIR else types.error.ENOTDIR)
      } else       if ((MODE == types.file_mode.MODE_R || MODE == types.file_mode.MODE_RW) && ! pr(USER, INODE.meta) || (MODE == types.file_mode.MODE_W || MODE == types.file_mode.MODE_RW) && ! pw(USER, INODE.meta)) {
        ERR := types.error.EACCESS
      }
    }
  }

  override def mkdir(PATH: path, MD: metadata, USER: user, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty) {
      ERR := types.error.EEXISTS
    } else {
      val INO = Ref[Int](ROOT_INO)
      val DENT = Ref[dentry](types.dentry.negdentry(PATH.last))
      val p: path = PATH.init
      walk(p, USER, INO, ERR)
      val INODE: inode = types.inode.uninit
      if (ERR.get == types.error.ESUCCESS) {
        may_create(INO.get, DENT.get, USER, INODE, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.check_commit(ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        val C_INODE: inode = types.inode.uninit
        afs.mkdir(MD, INODE, C_INODE, DENT, ERR)
      }
    }
  }

  override def open(PATH: path, MODE: file_mode, USER: user, FD: Ref[Int], ERR: Ref[error]): Unit = {
    val INO = Ref[Int](ROOT_INO)
    walk(PATH, USER, INO, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val INODE: inode = types.inode.uninit
      val ISDIR: Boolean = false
      may_open(INO.get, ISDIR, MODE, USER, INODE, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      OF(MAXINO) = types.file.mkfile(INO.get, MODE, 0)
      FD := MAXINO
      MAXINO = MAXINO + 1
    }
  }

  def put_inode(DEL_INODE: inode, ERR: Ref[error]): Unit = {
    if (ERR.get == types.error.ESUCCESS && ! is_open(DEL_INODE.ino, OF)) {
      afs.evict(DEL_INODE, ERR)
    }
  }

  override def read(FD: Int, USER: user, BUF: buffer, N: Ref[Int], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    if (! OF.contains(FD)) {
      ERR := types.error.EBADFD
    } else     if (OF(FD).mode != types.file_mode.MODE_R && OF(FD).mode != types.file_mode.MODE_RW) {
      ERR := types.error.EBADFD
    } else {
      val INODE: inode = types.inode.uninit
      afs.iget(OF(FD).ino, INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (INODE.directory) {
          ERR := types.error.EISDIR
        } else {
          val START: Int = OF(FD).pos
          val END: Int = OF(FD).pos + N.get
          if (START <= INODE.size) {
            val TOTAL = Ref[Int](0)
            val DONE: Boolean = false
            read_loop(START, END, INODE, DONE, BUF, TOTAL, ERR)
            if (TOTAL.get != 0) {
              ERR := types.error.ESUCCESS
            }
            OF(FD).pos = OF(FD).pos + TOTAL.get
            N := TOTAL.get
          } else {
            N := 0
          }
        }
      }
    }
  }

  def read_block(START: Int, END: Int, INODE: inode, BUF: buffer, TOTAL: Ref[Int], DONE: Ref[Boolean], ERR: Ref[error]): Unit = {
    val PAGENO: Int = (START + TOTAL.get) / VFS_PAGE_SIZE
    val OFFSET: Int = (START + TOTAL.get) % VFS_PAGE_SIZE
    val N: Int = min(END - (START + TOTAL.get), VFS_PAGE_SIZE - OFFSET, INODE.size - (START + TOTAL.get))
    if (N != 0) {
      val PBUF: buffer = new buffer(VFS_PAGE_SIZE).fill(0.toByte)
      afs.readpage(INODE, PAGENO, PBUF, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        BUF.copy(PBUF, OFFSET, TOTAL.get, N)
        TOTAL := TOTAL.get + N
      }
    } else {
      DONE := true
    }
  }

  def read_loop(START: Int, END: Int, INODE: inode, RSYNCED: Boolean, BUF: buffer, TOTAL: Ref[Int], ERR: Ref[error]): Unit = {
    val DONE = Ref[Boolean](RSYNCED)
    while (ERR.get == types.error.ESUCCESS && DONE.get != true) {
      read_block(START, END, INODE, BUF, TOTAL, DONE, ERR)
    }
  }

  override def readdir(PATH: path, USER: user, NAMES: stringset, ERR: Ref[error]): Unit = {
    val INO = Ref[Int](ROOT_INO)
    walk(PATH, USER, INO, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val INODE: inode = types.inode.uninit
      val ISDIR: Boolean = true
      val MODE: file_mode = types.file_mode.MODE_R
      may_open(INO.get, ISDIR, MODE, USER, INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val NAMES0: stringset = new stringset()
        afs.readdir(INODE, NAMES0, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          NAMES := NAMES0
        }
      }
    }
  }

  override def readmeta(PATH: path, USER: user, MD: Ref[metadata], NLINK: Ref[Int], SIZE: Ref[Int], ERR: Ref[error]): Unit = {
    val INO = Ref[Int](ROOT_INO)
    walk(PATH, USER, INO, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val INODE: inode = types.inode.uninit
      afs.iget(INO.get, INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (pr(USER, INODE.meta)) {
          MD := INODE.meta
          SIZE := INODE.size
          if (INODE.directory) {
            NLINK := INODE.nsubdirs + 2
          } else {
            NLINK := INODE.nlink
          }
        } else {
          ERR := types.error.EACCESS
        }
      }
    }
  }

  override def recover(DOSYNC: Boolean, ERR: Ref[error]): Unit = {
    OF.clear
    MAXINO = 0
    afs.recovery(DOSYNC, ERR)
  }

  override def rename(PATH: path, PATH_ : path, USER: user, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty || (PATH_.isEmpty || ⊑(PATH, PATH_))) {
      ERR := types.error.EACCESS
    } else {
      val OLD_DENT = Ref[dentry](types.dentry.uninit)
      val OLD_INODE: inode = types.inode.uninit
      val INODE: inode = types.inode.uninit
      val DEL_INODE: inode = types.inode.uninit
      val ISDIR = Ref[Boolean](helpers.scala.Boolean.uninit)
      val DENT0 = Ref[dentry](types.dentry.negdentry(PATH.last))
      val N = Ref[Int](ROOT_INO)
      val PATH1: path = PATH.init
      walk(PATH1, USER, N, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val ISRENAME: Boolean = true
        may_delete(N.get, ISRENAME, USER, INODE, DENT0, DEL_INODE, ISDIR, ERR)
      }
      OLD_INODE := INODE.deepCopy
      OLD_DENT := DENT0.get
      val OLD_DENT_INODE: inode = types.inode.uninit
      OLD_DENT_INODE := DEL_INODE.deepCopy
      val NEW_DENT = Ref[dentry](types.dentry.uninit)
      val NEW_DENT_INODE: inode = types.inode.uninit
      val NEW_INODE: inode = types.inode.uninit
      if (ERR.get == types.error.ESUCCESS) {
        val DENT = Ref[dentry](types.dentry.negdentry(PATH_.last))
        val INO = Ref[Int](ROOT_INO)
        val PATH: path = PATH_.init
        walk(PATH, USER, INO, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          may_lookup(INO.get, USER, INODE, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          afs.lookup(INO.get, DENT, ERR)
          if (ERR.get == types.error.ENOENT) {
            may_create(INO.get, DENT.get, USER, INODE, ERR)
          } else           if (ERR.get == types.error.ESUCCESS) {
            may_delete_check(INO.get, USER, INODE, ISDIR, DENT, DEL_INODE, ERR)
          }
        }
        NEW_INODE := INODE
        NEW_DENT := DENT.get
        NEW_DENT_INODE := DEL_INODE
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.check_commit(ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        val DENT: dentry = NEW_DENT.get
        afs.rename(OLD_INODE, NEW_INODE, OLD_DENT_INODE, NEW_DENT_INODE, OLD_DENT, NEW_DENT, ERR)
        if (DENT.isInstanceOf[types.dentry.mkdentry]) {
          put_inode(NEW_DENT_INODE, ERR)
        }
      }
    }
  }

  override def rmdir(PATH: path, USER: user, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty) {
      ERR := types.error.EACCESS
    } else {
      val INO = Ref[Int](ROOT_INO)
      val DENT = Ref[dentry](types.dentry.negdentry(PATH.last))
      val p: path = PATH.init
      walk(p, USER, INO, ERR)
      val DEL_INODE: inode = types.inode.uninit
      val INODE: inode = types.inode.uninit
      if (ERR.get == types.error.ESUCCESS) {
        val ISDIR = Ref[Boolean](true)
        may_delete_check(INO.get, USER, INODE, ISDIR, DENT, DEL_INODE, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.check_commit(ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.rmdir(INODE, DEL_INODE, DENT, ERR)
        put_inode(DEL_INODE, ERR)
      }
    }
  }

  override def seek(FD: Int, WHENCE: seekflag, USER: user, N: Ref[Int], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    if (! OF.contains(FD)) {
      ERR := types.error.EBADFD
    } else {
      if (WHENCE == types.seekflag.SEEK_CUR) {
        N := OF(FD).pos + N.get
      } else       if (WHENCE == types.seekflag.SEEK_END) {
        val INODE: inode = types.inode.uninit
        afs.iget(OF(FD).ino, INODE, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          N := INODE.size + N.get
        }
      }
      if (ERR.get == types.error.ESUCCESS) {
        OF(FD).pos = N.get
      }
    }
  }

  override def sync(ERR: Ref[error]): Unit = {
    afs.sync(ERR)
  }

  override def truncate(PATH: path, N: Int, USER: user, ERR: Ref[error]): Unit = {
    val INO = Ref[Int](ROOT_INO)
    walk(PATH, USER, INO, ERR)
    val INODE: inode = types.inode.uninit
    if (ERR.get == types.error.ESUCCESS) {
      val MODE: file_mode = types.file_mode.MODE_W
      val ISDIR: Boolean = false
      may_open(INO.get, ISDIR, MODE, USER, INODE, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      afs.check_commit(ERR)
    }
    var PAGENO: Int = 0
    if (ERR.get == types.error.ESUCCESS) {
      PAGENO = INODE.size / VFS_PAGE_SIZE
    }
    if (ERR.get == types.error.ESUCCESS) {
      val PBUF_OPT = Ref[buffer_opt](types.buffer_opt.none)
      val MODIFIED = Ref[Boolean](helpers.scala.Boolean.uninit)
      afs.truncate(N, PAGENO, PBUF_OPT, INODE, MODIFIED, ERR)
    }
  }

  override def unlink(PATH: path, USER: user, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty) {
      ERR := types.error.EACCESS
    } else {
      val INO = Ref[Int](ROOT_INO)
      val DENT = Ref[dentry](types.dentry.negdentry(PATH.last))
      val p: path = PATH.init
      walk(p, USER, INO, ERR)
      val DEL_INODE: inode = types.inode.uninit
      val INODE: inode = types.inode.uninit
      if (ERR.get == types.error.ESUCCESS) {
        val ISDIR = Ref[Boolean](false)
        may_delete_check(INO.get, USER, INODE, ISDIR, DENT, DEL_INODE, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.check_commit(ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.unlink(INODE, DEL_INODE, DENT, ERR)
        put_inode(DEL_INODE, ERR)
      }
    }
  }

  def walk(PATH: path, USER: user, INO: Ref[Int], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    var p: path = PATH
    while (! p.isEmpty && ERR.get == types.error.ESUCCESS) {
      val INODE: inode = types.inode.uninit
      may_lookup(INO.get, USER, INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val DENT = Ref[dentry](types.dentry.negdentry(p.head))
        afs.lookup(INO.get, DENT, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          INO := DENT.get.ino
          p = p.tail
        }
      }
    }
  }

  override def write(FD: Int, BUF: buffer, USER: user, N: Ref[Int], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    if (! OF.contains(FD)) {
      ERR := types.error.EBADFD
    } else     if (OF(FD).mode != types.file_mode.MODE_W && OF(FD).mode != types.file_mode.MODE_RW) {
      ERR := types.error.EBADFD
    } else     if (N.get != 0) {
      val INODE: inode = types.inode.uninit
      afs.iget(OF(FD).ino, INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (INODE.directory) {
          ERR := types.error.EISDIR
        } else {
          var PAGENO: Int = 0
          if (ERR.get == types.error.ESUCCESS) {
            PAGENO = INODE.size / VFS_PAGE_SIZE
          }
          if (ERR.get == types.error.ESUCCESS) {
            val MODIFIED = Ref[Boolean](helpers.scala.Boolean.uninit)
            val PBUF_OPT = Ref[buffer_opt](types.buffer_opt.none)
            afs.truncate(INODE.size, PAGENO, PBUF_OPT, INODE, MODIFIED, ERR)
          }
          val START: Int = OF(FD).pos
          var END: Int = OF(FD).pos + N.get
          val TOTAL = Ref[Int](0)
          if (ERR.get == types.error.ESUCCESS) {
            write_loop(START, INODE, END, BUF, TOTAL, ERR)
          }
          if (TOTAL.get != 0) {
            ERR := types.error.ESUCCESS
          }
          if (ERR.get == types.error.ESUCCESS) {
            N := TOTAL.get
            END = START + TOTAL.get
            val SIZE: Int = INODE.size
            if (TOTAL.get != 0 && SIZE < END) {
              INODE.size = END
              afs.check_commit(ERR)
              if (ERR.get == types.error.ESUCCESS) {
                afs.write_inode(INODE, ERR)
              }
            }
            if (ERR.get != types.error.ESUCCESS) {
              N := (if (START <= SIZE) SIZE - START else 0)
              ERR := types.error.ESUCCESS
            }
            OF(FD).pos = START + N.get
          }
        }
      }
    }
  }

  def write_block(START: Int, INODE: inode, BUF: buffer, END: Int, TOTAL: Ref[Int], DONE: Ref[Boolean], ERR: Ref[error]): Unit = {
    val PAGENO: Int = (START + TOTAL.get) / VFS_PAGE_SIZE
    val OFFSET: Int = (START + TOTAL.get) % VFS_PAGE_SIZE
    val N: Int = min(END - (START + TOTAL.get), VFS_PAGE_SIZE - OFFSET)
    if (N != 0) {
      writepage(INODE, PAGENO, BUF, TOTAL.get, OFFSET, N, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        TOTAL := TOTAL.get + N
      }
    } else {
      DONE := true
    }
  }

  def write_loop(START: Int, INODE: inode, END: Int, BUF: buffer, TOTAL: Ref[Int], ERR: Ref[error]): Unit = {
    val DONE = Ref[Boolean](false)
    while (ERR.get == types.error.ESUCCESS && DONE.get != true) {
      write_block(START, INODE, BUF, END, TOTAL, DONE, ERR)
    }
  }

  override def writemeta(PATH: path, MD: metadata, USER: user, ERR: Ref[error]): Unit = {
    val INO = Ref[Int](ROOT_INO)
    walk(PATH, USER, INO, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val INODE: inode = types.inode.uninit
      afs.iget(INO.get, INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (pw(USER, INODE.meta)) {
          INODE.meta = MD
          afs.check_commit(ERR)
          if (ERR.get == types.error.ESUCCESS) {
            afs.write_inode(INODE, ERR)
          }
        } else {
          ERR := types.error.EACCESS
        }
      }
    }
  }

  def writepage(INODE: inode, PAGENO: Int, BUF: buffer, TOTAL: Int, OFFSET: Int, N: Int, ERR: Ref[error]): Unit = {
    val PBUF: buffer = new buffer(VFS_PAGE_SIZE).fill(0.toByte)
    afs.readpage(INODE, PAGENO, PBUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      afs.check_commit(ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      PBUF.copy(BUF, TOTAL, OFFSET, N)
      afs.writepage(INODE, PAGENO, PBUF, ERR)
    }
  }

}

