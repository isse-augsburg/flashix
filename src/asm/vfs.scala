// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error
import types.file_mode.file_mode
import types.seekflag.seekflag

class vfs_asm(val OF : open_files, var MAXINO : Int, val afs : afs_interface)(implicit _algebraic_implicit: algebraic.Algebraic) extends posix_interface {
  import _algebraic_implicit._

  override def posix_close(FD: Int, USER: Byte, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    if (! OF.contains(FD))
      ERR := types.error.EBADFD
    else {
      val DEL_INO: Int = OF(FD).ino
      OF -= FD
      vfs_put_inode(DEL_INO, ERR)
    }
  }

  override def posix_create(PATH: path, MD: metadata, USER: Byte, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty)
      ERR := types.error.EEXISTS
    else {
      val INO = new Ref[Int](ROOT_INO)
      val DENT = new Ref[dentry](types.dentry.negdentry(PATH.last))
      val p: path = PATH.init
      vfs_walk(p, USER, INO, ERR)
      val INODE: inode = types.inode.uninit
      if (ERR.get == types.error.ESUCCESS) {
        vfs_may_create(INO.get, DENT.get, USER, INODE, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.afs_check_commit(ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.afs_create(INODE, MD, DENT, ERR)
      }
    }
  }

  override def posix_format(N: Int, MD: metadata, ERR: Ref[error]): Unit = {
    OF.clear
    MAXINO = 0
    afs.afs_format(N, MD, ERR)
  }

  override def posix_link(PATH: path, PATH_ : path, USER: Byte, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty || PATH_.isEmpty)
      ERR := types.error.EISDIR
    else {
      var OLD_DENT: dentry = types.dentry.uninit
      val INODE: inode = types.inode.uninit
      val DENT0 = new Ref[dentry](types.dentry.negdentry(PATH.last))
      val FD = new Ref[Int](ROOT_INO)
      val PATH1: path = PATH.init
      vfs_walk(PATH1, USER, FD, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        vfs_may_link(FD.get, USER, INODE, DENT0, ERR)
      }
      OLD_DENT = DENT0.get
      val NEW_DENT = new Ref[dentry](types.dentry.uninit)
      var NEW_INO: Int = 0
      if (ERR.get == types.error.ESUCCESS) {
        val INO = new Ref[Int](ROOT_INO)
        val PATH: path = PATH_.init
        vfs_walk(PATH, USER, INO, ERR)
        val DENT: dentry = types.dentry.negdentry(PATH_.last)
        if (ERR.get == types.error.ESUCCESS) {
          vfs_may_create(INO.get, DENT, USER, INODE, ERR)
        }
        NEW_DENT := DENT
        NEW_INO = INO.get
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.afs_check_commit(ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.afs_link(NEW_INO, OLD_DENT, NEW_DENT, ERR)
      }
    }
  }

  override def posix_mkdir(PATH: path, MD: metadata, USER: Byte, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty)
      ERR := types.error.EEXISTS
    else {
      val INO = new Ref[Int](ROOT_INO)
      val DENT = new Ref[dentry](types.dentry.negdentry(PATH.last))
      val p: path = PATH.init
      vfs_walk(p, USER, INO, ERR)
      val INODE: inode = types.inode.uninit
      if (ERR.get == types.error.ESUCCESS) {
        vfs_may_create(INO.get, DENT.get, USER, INODE, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.afs_check_commit(ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.afs_mkdir(INODE, MD, DENT, ERR)
      }
    }
  }

  override def posix_open(PATH: path, MODE: file_mode, USER: Byte, FD: Ref[Int], ERR: Ref[error]): Unit = {
    val INO = new Ref[Int](ROOT_INO)
    vfs_walk(PATH, USER, INO, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val INODE: inode = types.inode.uninit
      val ISDIR: Boolean = false
      vfs_may_open(INO.get, ISDIR, MODE, USER, INODE, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      OF(MAXINO) = types.file.mkfile(INO.get, MODE, 0)
      FD := MAXINO
      MAXINO = MAXINO + 1
    }
  }

  override def posix_read(FD: Int, USER: Byte, BUF: buffer, N: Ref[Int], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    if (! OF.contains(FD))
      ERR := types.error.EBADFD
    else     if (OF(FD).mode != types.file_mode.MODE_R && OF(FD).mode != types.file_mode.MODE_RW)
      ERR := types.error.EBADFD
    else {
      val INODE: inode = types.inode.uninit
      afs.afs_iget(OF(FD).ino, INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (INODE.directory)
          ERR := types.error.EISDIR
        else {
          val TOTAL = new Ref[Int](0)
          val END: Int = OF(FD).pos + N.get
          val START: Int = OF(FD).pos
          if (START <= INODE.size) {
            val DONE: Boolean = false
            vfs_read_loop(START, END, INODE, DONE, BUF, TOTAL, ERR)
            if (TOTAL.get != 0)
              ERR := types.error.ESUCCESS
            
            OF(FD).pos = OF(FD).pos + TOTAL.get
            N := TOTAL.get
          } else
            N := 0
        }
      }
    }
  }

  override def posix_readdir(PATH: path, USER: Byte, NAMES: stringset, ERR: Ref[error]): Unit = {
    val INO = new Ref[Int](ROOT_INO)
    vfs_walk(PATH, USER, INO, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val INODE: inode = types.inode.uninit
      val MODE: file_mode = types.file_mode.MODE_R
      val ISDIR: Boolean = true
      vfs_may_open(INO.get, ISDIR, MODE, USER, INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        afs.afs_readdir(INODE, NAMES, ERR)
      }
    }
  }

  override def posix_readmeta(PATH: path, USER: Byte, MD: Ref[metadata], NLINK: Ref[Int], SIZE: Ref[Int], ERR: Ref[error]): Unit = {
    val INO = new Ref[Int](ROOT_INO)
    vfs_walk(PATH, USER, INO, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val INODE: inode = types.inode.uninit
      afs.afs_iget(INO.get, INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (pr(USER, INODE.meta)) {
          NLINK := INODE.nlink
          SIZE := INODE.size
          MD := INODE.meta
        } else
          ERR := types.error.EACCESS
      }
    }
  }

  override def posix_recover(ERR: Ref[error]): Unit = {
    OF.clear
    MAXINO = 0
    afs.afs_recovery(ERR)
  }

  override def posix_rename(PATH: path, PATH_ : path, USER: Byte, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty || (PATH_.isEmpty || ⊑(PATH, PATH_)))
      ERR := types.error.EACCESS
    else {
      val OLD_DENT = new Ref[dentry](types.dentry.uninit)
      val NEW_DENT = new Ref[dentry](types.dentry.uninit)
      var NEW_INO: Int = 0
      var OLD_INO: Int = 0
      val INODE: inode = types.inode.uninit
      val ISDIR = new Ref[Boolean](helpers.scala.Boolean.uninit)
      val DENT0 = new Ref[dentry](types.dentry.negdentry(PATH.last))
      val FD = new Ref[Int](ROOT_INO)
      val PATH1: path = PATH.init
      vfs_walk(PATH1, USER, FD, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val ISRENAME: Boolean = true
        vfs_may_delete(FD.get, ISRENAME, USER, INODE, DENT0, ISDIR, ERR)
      }
      OLD_INO = FD.get
      OLD_DENT := DENT0.get
      if (ERR.get == types.error.ESUCCESS) {
        val INO = new Ref[Int](ROOT_INO)
        val DENT = new Ref[dentry](types.dentry.negdentry(PATH_.last))
        val PATH: path = PATH_.init
        vfs_walk(PATH, USER, INO, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          vfs_may_lookup(INO.get, USER, INODE, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          afs.afs_lookup(INO.get, DENT, ERR)
          if (ERR.get == types.error.ENOENT) {
            vfs_may_create(INO.get, DENT.get, USER, INODE, ERR)
          } else           if (ERR.get == types.error.ESUCCESS) {
            vfs_may_delete_check(INO.get, USER, INODE, ISDIR, DENT, ERR)
          }
        }
        NEW_DENT := DENT.get
        NEW_INO = INO.get
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.afs_check_commit(ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        val DENT: dentry = NEW_DENT.get
        afs.afs_rename(OLD_INO, NEW_INO, OLD_DENT, NEW_DENT, ERR)
        if (DENT.isInstanceOf[types.dentry.mkdentry]) {
          val DEL_INO: Int = DENT.ino
          vfs_put_inode(DEL_INO, ERR)
        }
      }
    }
  }

  override def posix_rmdir(PATH: path, USER: Byte, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty)
      ERR := types.error.EACCESS
    else {
      val INO = new Ref[Int](ROOT_INO)
      val DENT = new Ref[dentry](types.dentry.negdentry(PATH.last))
      val p: path = PATH.init
      vfs_walk(p, USER, INO, ERR)
      val INODE: inode = types.inode.uninit
      if (ERR.get == types.error.ESUCCESS) {
        val ISDIR = new Ref[Boolean](true)
        vfs_may_delete_check(INO.get, USER, INODE, ISDIR, DENT, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.afs_check_commit(ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        val DEL_INO: Int = DENT.get.ino
        afs.afs_rmdir(INODE, DENT, ERR)
        vfs_put_inode(DEL_INO, ERR)
      }
    }
  }

  override def posix_seek(FD: Int, WHENCE: seekflag, USER: Byte, N: Ref[Int], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    if (! OF.contains(FD))
      ERR := types.error.EBADFD
    else {
      if (WHENCE == types.seekflag.SEEK_CUR)
        N := OF(FD).pos + N.get
      else       if (WHENCE == types.seekflag.SEEK_END) {
        val INODE: inode = types.inode.uninit
        afs.afs_iget(OF(FD).ino, INODE, ERR)
        if (ERR.get == types.error.ESUCCESS)
          N := INODE.size + N.get
        
      }
      if (ERR.get == types.error.ESUCCESS)
        OF(FD).pos = N.get
      
    }
  }

  override def posix_truncate(PATH: path, N: Int, USER: Byte, ERR: Ref[error]): Unit = {
    val INO = new Ref[Int](ROOT_INO)
    vfs_walk(PATH, USER, INO, ERR)
    val INODE: inode = types.inode.uninit
    if (ERR.get == types.error.ESUCCESS) {
      val MODE: file_mode = types.file_mode.MODE_W
      val ISDIR: Boolean = false
      vfs_may_open(INO.get, ISDIR, MODE, USER, INODE, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      afs.afs_check_commit(ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      afs.afs_truncate(INODE, N, ERR)
    }
  }

  override def posix_unlink(PATH: path, USER: Byte, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty)
      ERR := types.error.EACCESS
    else {
      val INO = new Ref[Int](ROOT_INO)
      val DENT = new Ref[dentry](types.dentry.negdentry(PATH.last))
      val p: path = PATH.init
      vfs_walk(p, USER, INO, ERR)
      val INODE: inode = types.inode.uninit
      if (ERR.get == types.error.ESUCCESS) {
        val ISDIR = new Ref[Boolean](false)
        vfs_may_delete_check(INO.get, USER, INODE, ISDIR, DENT, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.afs_check_commit(ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        val DEL_INO: Int = DENT.get.ino
        afs.afs_unlink(INODE, DENT, ERR)
        vfs_put_inode(DEL_INO, ERR)
      }
    }
  }

  override def posix_write(FD: Int, BUF0: buffer, USER: Byte, N: Ref[Int], ERR: Ref[error]): Unit = {
    val BUF: buffer = BUF0.deepCopy
    ERR := types.error.ESUCCESS
    if (! OF.contains(FD))
      ERR := types.error.EBADFD
    else     if (OF(FD).mode != types.file_mode.MODE_W && OF(FD).mode != types.file_mode.MODE_RW)
      ERR := types.error.EBADFD
    else     if (N.get != 0) {
      val INODE: inode = types.inode.uninit
      afs.afs_iget(OF(FD).ino, INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (INODE.directory)
          ERR := types.error.EISDIR
        else {
          if (ERR.get == types.error.ESUCCESS) {
            afs.afs_truncate(INODE, INODE.size, ERR)
          }
          val TOTAL = new Ref[Int](0)
          var END: Int = OF(FD).pos + N.get
          val START: Int = OF(FD).pos
          if (ERR.get == types.error.ESUCCESS) {
            val DONE: Boolean = false
            vfs_write_loop(START, INODE, END, DONE, BUF, TOTAL, ERR)
          }
          if (TOTAL.get != 0)
            ERR := types.error.ESUCCESS
          
          if (ERR.get == types.error.ESUCCESS) {
            N := TOTAL.get
            END = START + TOTAL.get
            val SIZE: Int = INODE.size
            if (TOTAL.get != 0 && SIZE < END) {
              INODE.size = END
              afs.afs_check_commit(ERR)
              if (ERR.get == types.error.ESUCCESS) {
                afs.afs_write_inode(INODE, ERR)
              }
            }
            if (ERR.get != types.error.ESUCCESS) {
              ERR := types.error.ESUCCESS
              N := (if (START <= SIZE) SIZE - START else 0)
            }
            OF(FD).pos = START + N.get
          }
        }
      }
    }
  }

  override def posix_writemeta(PATH: path, MD: metadata, USER: Byte, ERR: Ref[error]): Unit = {
    val INO = new Ref[Int](ROOT_INO)
    vfs_walk(PATH, USER, INO, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val INODE: inode = types.inode.uninit
      afs.afs_iget(INO.get, INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (pw(USER, INODE.meta)) {
          INODE.meta = MD
          afs.afs_check_commit(ERR)
          if (ERR.get == types.error.ESUCCESS) {
            afs.afs_write_inode(INODE, ERR)
          }
        } else
          ERR := types.error.EACCESS
      }
    }
  }

  private def vfs_may_create(INO: Int, DENT0: dentry, USER: Byte, INODE: inode, ERR: Ref[error]): Unit = {
    val DENT = new Ref[dentry](DENT0)
    afs.afs_iget(INO, INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (! INODE.directory)
        ERR := types.error.ENOTDIR
      else       if (! px(USER, INODE.meta) || ! pw(USER, INODE.meta))
        ERR := types.error.EACCESS
      else {
        afs.afs_lookup(INO, DENT, ERR)
        if (ERR.get == types.error.ESUCCESS)
          ERR := types.error.EEXISTS
        else         if (ERR.get == types.error.ENOENT)
          ERR := types.error.ESUCCESS
        
      }
    }
  }

  private def vfs_may_delete(INO: Int, ISRENAME: Boolean, USER: Byte, INODE: inode, DENT: Ref[dentry], ISDIR: Ref[Boolean], ERR: Ref[error]): Unit = {
    afs.afs_iget(INO, INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (! INODE.directory)
        ERR := types.error.ENOTDIR
      else       if (! px(USER, INODE.meta) || ! pw(USER, INODE.meta))
        ERR := types.error.EACCESS
      else {
        afs.afs_lookup(INO, DENT, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          val DENT_INODE: inode = types.inode.uninit
          afs.afs_iget(DENT.get.ino, DENT_INODE, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            ISDIR := DENT_INODE.directory
            if (ISDIR.get) {
              if (DENT_INODE.ino == ROOT_INO)
                ERR := types.error.EACCESS
              else               if (ISRENAME && ! pw(USER, DENT_INODE.meta))
                ERR := types.error.EACCESS
              else               if (ISRENAME != true && DENT_INODE.size != 0)
                ERR := types.error.ENOTEMPTY
              
            }
          }
        }
      }
    }
  }

  private def vfs_may_delete_check(INO: Int, USER: Byte, INODE: inode, ISDIR: Ref[Boolean], DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    val ISDIR0: Boolean = ISDIR.get
    val ISRENAME: Boolean = false
    vfs_may_delete(INO, ISRENAME, USER, INODE, DENT, ISDIR, ERR)
    if (ISDIR0 && ISDIR.get != true)
      ERR := types.error.ENOTDIR
    
    if (ISDIR0 != true && ISDIR.get)
      ERR := types.error.EISDIR
    
  }

  private def vfs_may_link(INO: Int, USER: Byte, INODE: inode, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    afs.afs_iget(INO, INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (! INODE.directory)
        ERR := types.error.ENOTDIR
      else       if (! px(USER, INODE.meta))
        ERR := types.error.EACCESS
      else {
        afs.afs_lookup(INO, DENT, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          afs.afs_iget(DENT.get.ino, INODE, ERR)
        }
        if (ERR.get == types.error.ESUCCESS && INODE.directory)
          ERR := types.error.EISDIR
        
      }
    }
  }

  private def vfs_may_lookup(INO: Int, USER: Byte, INODE: inode, ERR: Ref[error]): Unit = {
    afs.afs_iget(INO, INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (! INODE.directory)
        ERR := types.error.ENOENT
      else       if (! px(USER, INODE.meta))
        ERR := types.error.EACCESS
      
    }
  }

  private def vfs_may_open(INO: Int, ISDIR: Boolean, MODE: file_mode, USER: Byte, INODE: inode, ERR: Ref[error]): Unit = {
    afs.afs_iget(INO, INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (! (INODE.directory == ISDIR))
        ERR := (if (INODE.directory) types.error.EISDIR else types.error.ENOTDIR)
      else       if ((MODE == types.file_mode.MODE_R || MODE == types.file_mode.MODE_RW) && ! pr(USER, INODE.meta) || (MODE == types.file_mode.MODE_W || MODE == types.file_mode.MODE_RW) && ! pw(USER, INODE.meta))
        ERR := types.error.EACCESS
      
    }
  }

  private def vfs_put_inode(DEL_INO: Int, ERR: Ref[error]): Unit = {
    if (ERR.get == types.error.ESUCCESS && ! is_open(DEL_INO, OF)) {
      val DEL_INODE: inode = types.inode.uninit
      afs.afs_iget(DEL_INO, DEL_INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        afs.afs_evict(DEL_INODE, ERR)
      }
    }
  }

  private def vfs_read_block(START: Int, END: Int, INODE: inode, BUF: buffer, TOTAL: Ref[Int], DONE: Ref[Boolean], ERR: Ref[error]): Unit = {
    val PBUF: buffer = new buffer(VFS_PAGE_SIZE).fill(0.toByte)
    val PAGENO: Int = (START + TOTAL.get) / VFS_PAGE_SIZE
    val OFFSET: Int = (START + TOTAL.get) % VFS_PAGE_SIZE
    val N: Int = min(END - (START + TOTAL.get), VFS_PAGE_SIZE - OFFSET, INODE.size - (START + TOTAL.get))
    if (N != 0) {
      afs.afs_readpage(INODE, PAGENO, PBUF, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        BUF.copy(PBUF, OFFSET, TOTAL.get, N)
        TOTAL := TOTAL.get + N
      }
    } else
      DONE := true
  }

  private def vfs_read_loop(START: Int, END: Int, INODE: inode, ISDIR: Boolean, BUF: buffer, TOTAL: Ref[Int], ERR: Ref[error]): Unit = {
    val DONE = new Ref[Boolean](ISDIR)
    while (ERR.get == types.error.ESUCCESS && DONE.get != true) {
      vfs_read_block(START, END, INODE, BUF, TOTAL, DONE, ERR)
    }
  }

  private def vfs_walk(PATH: path, USER: Byte, INO: Ref[Int], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    var p: path = PATH
    while (! p.isEmpty && ERR.get == types.error.ESUCCESS) {
      val INODE: inode = types.inode.uninit
      vfs_may_lookup(INO.get, USER, INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val DENT = new Ref[dentry](types.dentry.negdentry(p.head))
        afs.afs_lookup(INO.get, DENT, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          INO := DENT.get.ino
          p = p.tail
        }
      }
    }
  }

  private def vfs_write_block(START: Int, INODE: inode, END: Int, BUF: buffer, TOTAL: Ref[Int], DONE: Ref[Boolean], ERR: Ref[error]): Unit = {
    val PAGENO: Int = (START + TOTAL.get) / VFS_PAGE_SIZE
    val OFFSET: Int = (START + TOTAL.get) % VFS_PAGE_SIZE
    val N: Int = min(END - (START + TOTAL.get), VFS_PAGE_SIZE - OFFSET)
    if (N != 0) {
      vfs_writepage(INODE, PAGENO, BUF, TOTAL.get, OFFSET, N, ERR)
      if (ERR.get == types.error.ESUCCESS)
        TOTAL := TOTAL.get + N
      
    } else
      DONE := true
  }

  private def vfs_write_loop(START: Int, INODE: inode, END: Int, ISDIR: Boolean, BUF: buffer, TOTAL: Ref[Int], ERR: Ref[error]): Unit = {
    val DONE = new Ref[Boolean](ISDIR)
    while (ERR.get == types.error.ESUCCESS && DONE.get != true) {
      vfs_write_block(START, INODE, END, BUF, TOTAL, DONE, ERR)
    }
  }

  private def vfs_writepage(INODE: inode, PAGENO: Int, BUF: buffer, TOTAL: Int, OFFSET: Int, N: Int, ERR: Ref[error]): Unit = {
    val PBUF: buffer = new buffer()
    afs.afs_readpage(INODE, PAGENO, PBUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      afs.afs_check_commit(ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      PBUF.copy(BUF, TOTAL, OFFSET, N)
      afs.afs_writepage(INODE, PAGENO, PBUF, ERR)
    }
  }

}

