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
import types.file_mode.file_mode
import types.seekflag.seekflag

class Vfs(var MAXINO : Int, val OF : open_files, val afs : AfsInterface)(implicit _algebraic_implicit: algebraic.Algebraic, _procedures_implicit: proc.Procedures) extends PosixInterface {
  import _algebraic_implicit._
  import _procedures_implicit._

  override def close(FD: Int, USER: Int, ERR: Ref[error]): Unit = {
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

  override def create(PATH: path, MD: metadata, USER: Int, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty) {
      ERR := types.error.EEXISTS
    } else {
      val INO = Ref[Int](ROOT_INO)
      val PATH0: path = PATH.init
      walk(PATH0, USER, INO, ERR)
      val DENT = Ref[dentry](types.dentry.negdentry(PATH.last))
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

  override def format(N: Int, DOSYNC: Boolean, SIZE: Int, MD: metadata, ERR: Ref[error]): Unit = {
    OF.clear
    MAXINO = 0
    afs.format(N, DOSYNC, SIZE, MD, ERR)
  }

  override def fsync(FD: Int, ISDATASYNC: Boolean, USER: Int, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    if (! OF.contains(FD)) {
      ERR := types.error.EBADFD
    } else {
      val INODE: inode = types.inode.uninit
      afs.iget(OF(FD).ino, INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (INODE.directory) {
          ERR := types.error.EISDIR
        }
        if (ERR.get == types.error.ESUCCESS) {
          afs.fsync(INODE, ISDATASYNC, ERR)
        }
      }
    }
  }

  override def fsyncdir(PATH: path, ISDATASYNC: Boolean, USER: Int, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val INO = Ref[Int](ROOT_INO)
    walk(PATH, USER, INO, ERR)
    val INODE: inode = types.inode.uninit
    if (ERR.get == types.error.ESUCCESS) {
      val MODE: file_mode = types.file_mode.MODE_R
      val ISDIR: Boolean = true
      may_open(INO.get, ISDIR, MODE, USER, INODE, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      fsyncdir_rec(INODE, USER, ISDATASYNC, new nat_set(), ERR)
    }
  }

  def fsyncdir_rec(INODE: inode, USER: Int, ISDATASYNC: Boolean, SYNCED_INOS: nat_set, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val NAMES: stringset = new stringset()
    afs.readdir(INODE, NAMES, ERR)
    while (! NAMES.isEmpty && ERR.get == types.error.ESUCCESS) {
      val NAME: String = NAMES.head
      val DENT = Ref[dentry](types.dentry.negdentry(NAME))
      afs.lookup(INODE.ino, DENT, ERR)
      val C_INODE: inode = types.inode.uninit
      if (ERR.get == types.error.ESUCCESS) {
        val ISDIR: Boolean = true
        may_open(DENT.get.ino, ISDIR, types.file_mode.MODE_R, USER, C_INODE, ERR)
      }
      if (ERR.get == types.error.ESUCCESS && ! SYNCED_INOS.contains(C_INODE.ino)) {
        val nat_set_variable0: nat_set = SYNCED_INOS.deepCopy
        nat_set_variable0 += INODE.ino
        fsyncdir_rec(C_INODE, USER, ISDATASYNC, nat_set_variable0, ERR)
      } else       if (ERR.get == types.error.ENOTDIR) {
        afs.fsync(C_INODE, ISDATASYNC, ERR)
      }
      NAMES -= NAME
    }
  }

  override def link(PATH: path, PATH_ : path, USER: Int, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty || PATH_.isEmpty) {
      ERR := types.error.EISDIR
    } else {
      var OLD_DENT: dentry = types.dentry.uninit
      val INODE: inode = types.inode.uninit
      val C_INODE: inode = types.inode.uninit
      val INO1 = Ref[Int](ROOT_INO)
      val PATH1: path = PATH.init
      walk(PATH1, USER, INO1, ERR)
      val DENT1 = Ref[dentry](types.dentry.negdentry(PATH.last))
      if (ERR.get == types.error.ESUCCESS) {
        may_link(INO1.get, USER, INODE, DENT1, ERR)
      }
      C_INODE := INODE.deepCopy
      OLD_DENT = DENT1.get
      val NEW_DENT = Ref[dentry](types.dentry.uninit)
      val NEW_INODE: inode = types.inode.uninit
      if (ERR.get == types.error.ESUCCESS) {
        val INO = Ref[Int](ROOT_INO)
        val PATH0: path = PATH_.init
        walk(PATH0, USER, INO, ERR)
        val DENT: dentry = types.dentry.negdentry(PATH_.last)
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

  def may_create(INO: Int, DENT1: dentry, USER: Int, INODE: inode, ERR: Ref[error]): Unit = {
    val DENT = Ref[dentry](DENT1)
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

  def may_delete(INO: Int, ISRENAME: Boolean, USER: Int, INODE: inode, DENT: Ref[dentry], DENT_INODE: inode, ISDIR: Ref[Boolean], ERR: Ref[error]): Unit = {
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

  def may_delete_check(INO: Int, USER: Int, INODE: inode, ISDIR: Ref[Boolean], DENT: Ref[dentry], DENT_INODE: inode, ERR: Ref[error]): Unit = {
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

  def may_link(INO: Int, USER: Int, INODE: inode, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
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

  def may_lookup(INO: Int, USER: Int, INODE: inode, ERR: Ref[error]): Unit = {
    afs.iget(INO, INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (! INODE.directory) {
        ERR := types.error.ENOENT
      } else       if (! px(USER, INODE.meta)) {
        ERR := types.error.EACCESS
      }
    }
  }

  def may_open(INO: Int, ISDIR: Boolean, MODE: file_mode, USER: Int, INODE: inode, ERR: Ref[error]): Unit = {
    afs.iget(INO, INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (! (INODE.directory == ISDIR)) {
        ERR := (if (INODE.directory) types.error.EISDIR else types.error.ENOTDIR)
      } else       if ((MODE == types.file_mode.MODE_R || MODE == types.file_mode.MODE_RW) && ! pr(USER, INODE.meta) || (MODE == types.file_mode.MODE_W || MODE == types.file_mode.MODE_RW) && ! pw(USER, INODE.meta)) {
        ERR := types.error.EACCESS
      }
    }
  }

  override def mkdir(PATH: path, MD: metadata, USER: Int, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty) {
      ERR := types.error.EEXISTS
    } else {
      val INO = Ref[Int](ROOT_INO)
      val PATH0: path = PATH.init
      walk(PATH0, USER, INO, ERR)
      val DENT = Ref[dentry](types.dentry.negdentry(PATH.last))
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

  override def open(PATH: path, MODE: file_mode, USER: Int, FD: Ref[Int], ERR: Ref[error]): Unit = {
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

  override def read(FD: Int, USER: Int, BUF: buffer, N: Ref[Int], ERR: Ref[error]): Unit = {
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
          val TOTAL = Ref[Int](0)
          val END: Int = OF(FD).pos + N.get
          val START: Int = OF(FD).pos
          if (START <= INODE.size && START < END) {
            read_loop(INODE, START, END, BUF, TOTAL, ERR)
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

  def read_block(INODE: inode, START: Int, END: Int, BUF: buffer, TOTAL: Ref[Int], DONE: Ref[Boolean], ERR: Ref[error]): Unit = {
    val CURRENT_POS: Int = START + TOTAL.get
    val OFFSET = Ref[Int](0)
    val REST = Ref[Int](0)
    val PAGENO = Ref[Int](0)
    vfs_block_boundaries(CURRENT_POS, PAGENO, OFFSET, REST)
    val PBUF: buffer = vfspage
    val N: Int = min(END - CURRENT_POS, REST.get, INODE.size - CURRENT_POS)
    val EXISTS = Ref[Boolean](false)
    afs.readpage(INODE, PAGENO.get, PBUF, EXISTS, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      BUF.copy(PBUF, OFFSET.get, TOTAL.get, N)
      TOTAL := TOTAL.get + N
    }
    DONE := (START + TOTAL.get == END || START + TOTAL.get == INODE.size)
  }

  def read_loop(INODE: inode, START: Int, END: Int, BUF: buffer, TOTAL: Ref[Int], ERR: Ref[error]): Unit = {
    TOTAL := 0
    ERR := types.error.ESUCCESS
    val DONE = Ref[Boolean](START == INODE.size)
    while (ERR.get == types.error.ESUCCESS && DONE.get != true) {
      read_block(INODE, START, END, BUF, TOTAL, DONE, ERR)
    }
  }

  override def readdir(PATH: path, USER: Int, NAMES: stringset, ERR: Ref[error]): Unit = {
    val INO = Ref[Int](ROOT_INO)
    walk(PATH, USER, INO, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val INODE: inode = types.inode.uninit
      val MODE: file_mode = types.file_mode.MODE_R
      val ISDIR: Boolean = true
      may_open(INO.get, ISDIR, MODE, USER, INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        afs.readdir(INODE, NAMES, ERR)
      }
    }
  }

  override def readmeta(PATH: path, USER: Int, MD: Ref[metadata], NLINK: Ref[Int], SIZE: Ref[Int], ERR: Ref[error]): Unit = {
    val INO = Ref[Int](ROOT_INO)
    walk(PATH, USER, INO, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val INODE: inode = types.inode.uninit
      afs.iget(INO.get, INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (pr(USER, INODE.meta)) {
          SIZE := INODE.size
          MD := INODE.meta
          if (INODE.directory) {
            if (PATH.isEmpty) {
              NLINK := INODE.nsubdirs + 1
            } else {
              NLINK := INODE.nsubdirs + 2
            }
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

  override def rename(PATH: path, PATH_ : path, USER: Int, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty || (PATH_.isEmpty || ⊑(PATH, PATH_))) {
      ERR := types.error.EACCESS
    } else {
      val OLD_DENT = Ref[dentry](types.dentry.uninit)
      val OLD_INODE: inode = types.inode.uninit
      val INODE: inode = types.inode.uninit
      val DEL_INODE: inode = types.inode.uninit
      val ISDIR = Ref[Boolean](helpers.scala.Boolean.uninit)
      val INO1 = Ref[Int](ROOT_INO)
      val PATH1: path = PATH.init
      walk(PATH1, USER, INO1, ERR)
      val DENT1 = Ref[dentry](types.dentry.negdentry(PATH.last))
      if (ERR.get == types.error.ESUCCESS) {
        val ISRENAME: Boolean = true
        may_delete(INO1.get, ISRENAME, USER, INODE, DENT1, DEL_INODE, ISDIR, ERR)
      }
      OLD_INODE := INODE.deepCopy
      OLD_DENT := DENT1.get
      val OLD_DENT_INODE: inode = DEL_INODE
      val NEW_DENT = Ref[dentry](types.dentry.uninit)
      val NEW_DENT_INODE: inode = types.inode.uninit
      val NEW_INODE: inode = types.inode.uninit
      if (ERR.get == types.error.ESUCCESS) {
        val INO = Ref[Int](ROOT_INO)
        val PATH0: path = PATH_.init
        walk(PATH0, USER, INO, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          may_lookup(INO.get, USER, INODE, ERR)
        }
        val DENT = Ref[dentry](types.dentry.negdentry(PATH_.last))
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
        afs.rename(OLD_DENT_INODE, OLD_INODE, NEW_INODE, NEW_DENT_INODE, OLD_DENT, NEW_DENT, ERR)
        if (DENT.isInstanceOf[types.dentry.mkdentry]) {
          put_inode(NEW_DENT_INODE, ERR)
        }
      }
    }
  }

  override def rmdir(PATH: path, USER: Int, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty) {
      ERR := types.error.EACCESS
    } else {
      val INO = Ref[Int](ROOT_INO)
      val PATH0: path = PATH.init
      walk(PATH0, USER, INO, ERR)
      val DENT = Ref[dentry](types.dentry.negdentry(PATH.last))
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

  override def seek(FD: Int, WHENCE: seekflag, USER: Int, N: Ref[Int], ERR: Ref[error]): Unit = {
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

  override def truncate(PATH: path, N: Int, USER: Int, ERR: Ref[error]): Unit = {
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
    if (ERR.get == types.error.ESUCCESS) {
      val PBUF_OPT = Ref[buffer_opt](types.buffer_opt.none)
      afs.truncate(N, INODE, PBUF_OPT, ERR)
    }
  }

  override def unlink(PATH: path, USER: Int, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty) {
      ERR := types.error.EACCESS
    } else {
      val INO = Ref[Int](ROOT_INO)
      val PATH0: path = PATH.init
      walk(PATH0, USER, INO, ERR)
      val DENT = Ref[dentry](types.dentry.negdentry(PATH.last))
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

  def walk(PATH1: path, USER: Int, INO: Ref[Int], ERR: Ref[error]): Unit = {
    var PATH: path = PATH1
    ERR := types.error.ESUCCESS
    while (! PATH.isEmpty && ERR.get == types.error.ESUCCESS) {
      val INODE: inode = types.inode.uninit
      may_lookup(INO.get, USER, INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val DENT = Ref[dentry](types.dentry.negdentry(PATH.head))
        afs.lookup(INO.get, DENT, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          INO := DENT.get.ino
          PATH = PATH.tail
        }
      }
    }
  }

  override def write(FD: Int, BUF: buffer, USER: Int, N: Ref[Int], ERR: Ref[error]): Unit = {
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
          if (ERR.get == types.error.ESUCCESS) {
            val PBUF_OPT = Ref[buffer_opt](types.buffer_opt.none)
            afs.truncate(INODE.size, INODE, PBUF_OPT, ERR)
          }
          val WRITTEN = Ref[Int](0)
          var END: Int = OF(FD).pos + N.get
          val START: Int = OF(FD).pos
          if (ERR.get == types.error.ESUCCESS) {
            write_loop(INODE, START, END, BUF, WRITTEN, ERR)
            if (WRITTEN.get != 0) {
              ERR := types.error.ESUCCESS
            }
          }
          if (ERR.get == types.error.ESUCCESS) {
            END = START + WRITTEN.get
            val SIZE: Int = INODE.size
            if (SIZE < END) {
              afs.check_commit(ERR)
              if (ERR.get == types.error.ESUCCESS) {
                afs.write_size(INODE, END, ERR)
              }
            }
            if (ERR.get == types.error.ESUCCESS) {
              N := WRITTEN.get
            } else {
              ERR := types.error.ESUCCESS
              N := (if (START <= SIZE) SIZE - START else 0)
            }
            OF(FD).pos = START + N.get
          }
        }
      }
    }
  }

  def write_block(INODE: inode, START: Int, END: Int, BUF: buffer, WRITTEN: Ref[Int], DONE: Ref[Boolean], ERR: Ref[error]): Unit = {
    val OFFSET = Ref[Int](0)
    val REST = Ref[Int](0)
    val PAGENO = Ref[Int](0)
    val CURRENT_POS: Int = START + WRITTEN.get
    vfs_block_boundaries(CURRENT_POS, PAGENO, OFFSET, REST)
    val N: Int = min(END - CURRENT_POS, REST.get)
    writepage(INODE, PAGENO.get, BUF, WRITTEN.get, OFFSET.get, N, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      WRITTEN := WRITTEN.get + N
    }
    DONE := (START + WRITTEN.get == END)
  }

  def write_loop(INODE: inode, START: Int, END: Int, BUF: buffer, WRITTEN: Ref[Int], ERR: Ref[error]): Unit = {
    WRITTEN := 0
    ERR := types.error.ESUCCESS
    val DONE = Ref[Boolean](false)
    while (ERR.get == types.error.ESUCCESS && DONE.get != true) {
      write_block(INODE, START, END, BUF, WRITTEN, DONE, ERR)
    }
  }

  override def writemeta(PATH: path, MD: metadata, USER: Int, ERR: Ref[error]): Unit = {
    val INO = Ref[Int](ROOT_INO)
    walk(PATH, USER, INO, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val INODE: inode = types.inode.uninit
      afs.iget(INO.get, INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (pw(USER, INODE.meta)) {
          afs.check_commit(ERR)
          if (ERR.get == types.error.ESUCCESS) {
            afs.write_meta(INODE, MD, ERR)
          }
        } else {
          ERR := types.error.EACCESS
        }
      }
    }
  }

  def writepage(INODE: inode, PAGENO: Int, BUF: buffer, TOTAL: Int, OFFSET: Int, N: Int, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val PBUF: buffer = vfspage
    val EXISTS = Ref[Boolean](false)
    afs.readpage(INODE, PAGENO, PBUF, EXISTS, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      afs.check_commit(ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      PBUF.copy(BUF, TOTAL, OFFSET, N)
      afs.writepage(INODE, PAGENO, PBUF, ERR)
    }
  }

}
