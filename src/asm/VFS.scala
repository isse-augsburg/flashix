// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import sorts._
import types._
import types.error.error
import types.file_mode.file_mode
import types.seekflag.seekflag

class VFS(val OF : open_files, val afs : AFS)(implicit _algebraic_implicit: algebraic.Algebraic) extends POSIX {
  import _algebraic_implicit._

  private def may_create(INO: Int, USER: user, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    val INODE = new Ref[inode](inode.uninit)
    afs.afs_iget(INO, INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (! INODE.get.directory)
        ERR := types.error.ENOTDIR
      else {
        if (! px(USER, INODE.get.meta) || ! pw(USER, INODE.get.meta))
          ERR := types.error.EACCESS
        else {
          afs.afs_lookup(INO, DENT, ERR)
          if (ERR.get == types.error.ESUCCESS)
            ERR := types.error.EEXISTS
          else {
            if (ERR.get == types.error.ENOENT)
              ERR := types.error.ESUCCESS
            
          }
        }
      }
    }
  }

  private def may_delete(INO: Int, USER: user, ISRENAME: Boolean, DENT: Ref[dentry], ISDIR: Ref[Boolean], ERR: Ref[error]): Unit = {
    val INODE = new Ref[inode](inode.uninit)
    afs.afs_iget(INO, INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (! INODE.get.directory)
        ERR := types.error.ENOTDIR
      else {
        if (! px(USER, INODE.get.meta) || ! pw(USER, INODE.get.meta))
          ERR := types.error.EACCESS
        else {
          afs.afs_lookup(INO, DENT, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            afs.afs_iget(DENT.get.ino, INODE, ERR)
            if (ERR.get == types.error.ESUCCESS) {
              ISDIR := INODE.get.directory
              if (ISDIR.get) {
                if (INODE.get.ino == ROOT_INO)
                  ERR := types.error.EACCESS
                else {
                  if (ISRENAME && ! pw(USER, INODE.get.meta))
                    ERR := types.error.EACCESS
                  else {
                    if (ISRENAME != true && INODE.get.size != 0)
                      ERR := types.error.ENOTEMPTY
                    
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  private def may_delete_check(INO: Int, USER: user, DENT: Ref[dentry], ISDIR: Ref[Boolean], ERR: Ref[error]): Unit = {
    val ISDIR0: Boolean = ISDIR.get
    val ISRENAME: Boolean = false
    may_delete(INO, USER, ISRENAME, DENT, ISDIR, ERR)
    if (ISDIR0 && ISDIR.get != true)
      ERR := types.error.ENOTDIR
    
    if (ISDIR0 != true && ISDIR.get)
      ERR := types.error.EISDIR
    
  }

  private def may_link(INO: Int, USER: user, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    val INODE = new Ref[inode](inode.uninit)
    afs.afs_iget(INO, INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (! INODE.get.directory)
        ERR := types.error.ENOTDIR
      else {
        if (! px(USER, INODE.get.meta))
          ERR := types.error.EACCESS
        else {
          afs.afs_lookup(INO, DENT, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            afs.afs_iget(DENT.get.ino, INODE, ERR)
          }
          if (ERR.get == types.error.ESUCCESS && INODE.get.directory)
            ERR := types.error.EISDIR
          
        }
      }
    }
  }

  private def may_lookup(INO: Int, USER: user, ERR: Ref[error]): Unit = {
    val INODE = new Ref[inode](inode.uninit)
    afs.afs_iget(INO, INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (! INODE.get.directory)
        ERR := types.error.ENOENT
      else {
        if (! px(USER, INODE.get.meta))
          ERR := types.error.EACCESS
        
      }
    }
  }

  private def may_open(INO: Int, ISDIR: Boolean, MODE0: file_mode, USER: user, ERR: Ref[error]): Unit = {
    val INODE = new Ref[inode](inode.uninit)
    afs.afs_iget(INO, INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (! (INODE.get.directory == ISDIR))
        ERR := (if (INODE.get.directory) types.error.EISDIR else types.error.ENOTDIR)
      else {
        if ((MODE0 == types.file_mode.MODE_R || MODE0 == types.file_mode.MODE_RW) && ! pr(USER, INODE.get.meta) || (MODE0 == types.file_mode.MODE_W || MODE0 == types.file_mode.MODE_RW) && ! pw(USER, INODE.get.meta))
          ERR := types.error.EACCESS
        
      }
    }
  }

  override def posix_close(FD: Int, USER: user, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    if (! OF.contains(FD))
      ERR := types.error.EBADFD
    else {
      val DEL_INO: Int = OF(FD).ino
      OF -= FD
      put_inode(DEL_INO, ERR)
    }
  }

  override def posix_create(PATH: path, MD: metadata, USER: user, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty)
      ERR := types.error.EEXISTS
    else {
      val INO = new Ref[Int](ROOT_INO)
      val DENT = new Ref[dentry](types.dentry.negdentry(PATH.last))
      val path: path = PATH.init
      vfs_walk(path, USER, INO, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        may_create(INO.get, USER, DENT, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.afs_check_commit(ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.afs_create(INO.get, MD, DENT, ERR)
      }
    }
  }

  override def posix_format(N: Int, MD: metadata, ERR: Ref[error]): Unit = {
    OF.clear
    afs.afs_format(N, MD, ERR)
  }

  override def posix_link(PATH: path, PATH_ : path, USER: user, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty || PATH_.isEmpty)
      ERR := types.error.EISDIR
    else {
      var OLD_DENT: dentry = dentry.uninit
      val DENT0 = new Ref[dentry](types.dentry.negdentry(PATH.last))
      val INO0 = new Ref[Int](ROOT_INO)
      val PATH0: path = PATH.init
      vfs_walk(PATH0, USER, INO0, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        may_link(INO0.get, USER, DENT0, ERR)
      }
      
      OLD_DENT = DENT0.get
      val NEW_DENT = new Ref[dentry](dentry.uninit)
      var NEW_INO: Int = 0
      if (ERR.get == types.error.ESUCCESS) {
        val INO = new Ref[Int](ROOT_INO)
        val DENT = new Ref[dentry](types.dentry.negdentry(PATH_.last))
        val PATH: path = PATH_.init
        vfs_walk(PATH, USER, INO, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          may_create(INO.get, USER, DENT, ERR)
        }
        NEW_DENT := DENT.get
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

  override def posix_mkdir(PATH: path, MD: metadata, USER: user, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty)
      ERR := types.error.EEXISTS
    else {
      val INO = new Ref[Int](ROOT_INO)
      val DENT = new Ref[dentry](types.dentry.negdentry(PATH.last))
      val path: path = PATH.init
      vfs_walk(path, USER, INO, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        may_create(INO.get, USER, DENT, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.afs_check_commit(ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.afs_mkdir(INO.get, MD, DENT, ERR)
      }
    }
  }

  override def posix_open(PATH: path, MODE0: file_mode, USER: user, FD: Ref[Int], ERR: Ref[error]): Unit = {
    val INO = new Ref[Int](ROOT_INO)
    vfs_walk(PATH, USER, INO, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val ISDIR: Boolean = false
      may_open(INO.get, ISDIR, MODE0, USER, ERR)
    }
    if (ERR.get == types.error.ESUCCESS)
      Or(
      {
        ChooseNotin((OF).keys.toSeq, (N : Int) => {true}, (N : Int) =>
        {
          OF(N) = types.file.mkfile(INO.get, MODE0, 0)
          FD := N
        })
      },
      {
        ERR := types.error.ENOMEM
      })
    
  }

  override def posix_read(FD: Int, USER: user, N: Ref[Int], BUF: buffer, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    if (! OF.contains(FD))
      ERR := types.error.EBADFD
    else {
      if (OF(FD).mode != types.file_mode.MODE_R && OF(FD).mode != types.file_mode.MODE_RW)
        ERR := types.error.EBADFD
      else {
        val INODE = new Ref[inode](inode.uninit)
        afs.afs_iget(OF(FD).ino, INODE, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          if (INODE.get.directory)
            ERR := types.error.EISDIR
          else {
            val TOTAL = new Ref[Int](0)
            val END: Int = OF(FD).pos + N.get
            val START: Int = OF(FD).pos
            if (START <= INODE.get.size) {
              val DONE = new Ref[Boolean](false)
              vfs_read_loop(START, END, INODE.get, BUF, TOTAL, DONE, ERR)
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
  }

  override def posix_readdir(PATH: path, USER: user, NAMES: stringset, ERR: Ref[error]): Unit = {
    val INO = new Ref[Int](ROOT_INO)
    vfs_walk(PATH, USER, INO, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val INODE = new Ref[inode](inode.uninit)
      afs.afs_iget(INO.get, INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val MODE0: file_mode = types.file_mode.MODE_R
        val ISDIR: Boolean = true
        may_open(INO.get, ISDIR, MODE0, USER, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.afs_readdir(INO.get, NAMES, ERR)
      }
    }
  }

  override def posix_readmeta(PATH: path, USER: user, MD: Ref[metadata], NLINK: Ref[Int], SIZE: Ref[Int], ERR: Ref[error]): Unit = {
    val INO = new Ref[Int](ROOT_INO)
    vfs_walk(PATH, USER, INO, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val INODE = new Ref[inode](inode.uninit)
      afs.afs_iget(INO.get, INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (pr(USER, INODE.get.meta)) {
          SIZE := INODE.get.size
          MD := INODE.get.meta
          NLINK := INODE.get.nlink
        } else
          ERR := types.error.EACCESS
      }
    }
  }

  override def posix_rename(PATH: path, PATH_ : path, USER: user, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty || (PATH_.isEmpty || PATH_.startsWith(PATH)))
      ERR := types.error.EACCESS
    else {
      var OLD_INO: Int = 0
      val ISDIR = new Ref[Boolean](helpers.scala.Boolean.uninit)
      val OLD_DENT = new Ref[dentry](dentry.uninit)
      val DENT0 = new Ref[dentry](types.dentry.negdentry(PATH.last))
      val INO0 = new Ref[Int](ROOT_INO)
      val PATH0: path = PATH.init
      vfs_walk(PATH0, USER, INO0, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val ISRENAME: Boolean = true
        may_delete(INO0.get, USER, ISRENAME, DENT0, ISDIR, ERR)
      }
      OLD_INO = INO0.get
      OLD_DENT := DENT0.get
      val NEW_DENT = new Ref[dentry](dentry.uninit)
      var NEW_INO: Int = 0
      if (ERR.get == types.error.ESUCCESS) {
        val DENT = new Ref[dentry](types.dentry.negdentry(PATH_.last))
        val INO = new Ref[Int](ROOT_INO)
        val PATH: path = PATH_.init
        vfs_walk(PATH, USER, INO, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          may_lookup(INO.get, USER, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          afs.afs_lookup(INO.get, DENT, ERR)
          if (ERR.get == types.error.ENOENT) {
            may_create(INO.get, USER, DENT, ERR)
          } else {
            if (ERR.get == types.error.ESUCCESS) {
              may_delete_check(INO.get, USER, DENT, ISDIR, ERR)
            }
          }
        }
        NEW_INO = INO.get
        NEW_DENT := DENT.get
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.afs_check_commit(ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        val DENT: dentry = NEW_DENT.get
        afs.afs_rename(OLD_INO, NEW_INO, OLD_DENT, NEW_DENT, ERR)
        if (DENT.isInstanceOf[types.dentry.mkdentry]) {
          val DEL_INO: Int = DENT.ino
          put_inode(DEL_INO, ERR)
        }
      }
    }
  }

  override def posix_rmdir(PATH: path, USER: user, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty)
      ERR := types.error.EACCESS
    else {
      val INO = new Ref[Int](ROOT_INO)
      val DENT = new Ref[dentry](types.dentry.negdentry(PATH.last))
      val path: path = PATH.init
      vfs_walk(path, USER, INO, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val ISDIR = new Ref[Boolean](true)
        may_delete_check(INO.get, USER, DENT, ISDIR, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.afs_check_commit(ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        val DEL_INO: Int = DENT.get.ino
        afs.afs_rmdir(INO.get, DENT, ERR)
        put_inode(DEL_INO, ERR)
      }
    }
  }

  override def posix_seek(FD: Int, WHENCE: seekflag, USER: user, N: Ref[Int], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    if (! OF.contains(FD))
      ERR := types.error.EBADFD
    else {
      if (WHENCE == types.seekflag.SEEK_CUR)
        N := OF(FD).pos + N.get
      else {
        if (WHENCE == types.seekflag.SEEK_END) {
          val INODE = new Ref[inode](inode.uninit)
          afs.afs_iget(OF(FD).ino, INODE, ERR)
          if (ERR.get == types.error.ESUCCESS)
            N := INODE.get.size + N.get
          
        }
      }
      if (ERR.get == types.error.ESUCCESS)
        OF(FD).pos = N.get
      
    }
  }

  override def posix_truncate(PATH: path, N: Int, USER: user, ERR: Ref[error]): Unit = {
    val INO = new Ref[Int](ROOT_INO)
    vfs_walk(PATH, USER, INO, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val MODE0: file_mode = types.file_mode.MODE_W
      val ISDIR: Boolean = false
      may_open(INO.get, ISDIR, MODE0, USER, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      val INODE = new Ref[inode](inode.uninit)
      afs.afs_iget(INO.get, INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        afs.afs_check_commit(ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.afs_truncate(INODE.get, N, ERR)
      }
    }
  }

  override def posix_unlink(PATH: path, USER: user, ERR: Ref[error]): Unit = {
    if (PATH.isEmpty)
      ERR := types.error.EACCESS
    else {
      val INO = new Ref[Int](ROOT_INO)
      val DENT = new Ref[dentry](types.dentry.negdentry(PATH.last))
      val path: path = PATH.init
      vfs_walk(path, USER, INO, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val ISDIR = new Ref[Boolean](false)
        may_delete_check(INO.get, USER, DENT, ISDIR, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        afs.afs_check_commit(ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        val DEL_INO: Int = DENT.get.ino
        afs.afs_unlink(INO.get, DENT, ERR)
        put_inode(DEL_INO, ERR)
      }
    }
  }

  override def posix_write(FD: Int, BUF: buffer, USER: user, N: Ref[Int], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    if (! OF.contains(FD))
      ERR := types.error.EBADFD
    else {
      if (OF(FD).mode != types.file_mode.MODE_W && OF(FD).mode != types.file_mode.MODE_RW)
        ERR := types.error.EBADFD
      else {
        if (N.get != 0) {
          val INODE = new Ref[inode](inode.uninit)
          afs.afs_iget(OF(FD).ino, INODE, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            if (INODE.get.directory)
              ERR := types.error.EISDIR
            else {
              if (ERR.get == types.error.ESUCCESS) {
                afs.afs_truncate(INODE.get, INODE.get.size, ERR)
              }
              val TOTAL = new Ref[Int](0)
              var END: Int = OF(FD).pos + N.get
              val START: Int = OF(FD).pos
              if (ERR.get == types.error.ESUCCESS) {
                val DONE = new Ref[Boolean](false)
                vfs_write_loop(START, END, INODE.get, BUF, TOTAL, DONE, ERR)
              }
              if (TOTAL.get != 0)
                ERR := types.error.ESUCCESS
              
              if (ERR.get == types.error.ESUCCESS) {
                END = START + TOTAL.get
                N := TOTAL.get
                val SIZE: Int = INODE.get.size
                if (TOTAL.get != 0 && SIZE < END) {
                  INODE := INODE.get.copy(size = END)
                  afs.afs_check_commit(ERR)
                  if (ERR.get == types.error.ESUCCESS) {
                    afs.afs_write_inode(INODE.get, ERR)
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
    }
  }

  override def posix_writemeta(PATH: path, MD: metadata, USER: user, ERR: Ref[error]): Unit = {
    val INO = new Ref[Int](ROOT_INO)
    vfs_walk(PATH, USER, INO, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val INODE = new Ref[inode](inode.uninit)
      afs.afs_iget(INO.get, INODE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (pw(USER, INODE.get.meta)) {
          INODE := INODE.get.copy(meta = MD)
          afs.afs_check_commit(ERR)
          if (ERR.get == types.error.ESUCCESS) {
            afs.afs_write_inode(INODE.get, ERR)
          }
        } else
          ERR := types.error.EACCESS
      }
    }
  }

  private def put_inode(DEL_INO: Int, ERR: Ref[error]): Unit = {
    if (ERR.get == types.error.ESUCCESS && ! is_open(DEL_INO, OF)) {
      afs.afs_evict(DEL_INO, ERR)
    }
  }

  private def vfs_read_block(START: Int, END: Int, INODE: inode, BUF: buffer, TOTAL: Ref[Int], DONE: Ref[Boolean], ERR: Ref[error]): Unit = {
    val PAGENO: Int = (START + TOTAL.get) / VFS_PAGE_SIZE
    val OFFSET: Int = (START + TOTAL.get) % VFS_PAGE_SIZE
    val N: Int = min(END - (START + TOTAL.get), VFS_PAGE_SIZE - OFFSET, INODE.size - (START + TOTAL.get))
    if (N != 0) {
      val PBUF: buffer = new buffer()
      afs.afs_readpage(INODE, PAGENO, PBUF, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        BUF.copy(PBUF, OFFSET, TOTAL.get, N)
        TOTAL := TOTAL.get + N
      }
    } else
      DONE := true
  }

  private def vfs_read_loop(START: Int, END: Int, INODE: inode, BUF: buffer, TOTAL: Ref[Int], DONE: Ref[Boolean], ERR: Ref[error]): Unit = {
    while (ERR.get == types.error.ESUCCESS && DONE.get != true) {
      vfs_read_block(START, END, INODE, BUF, TOTAL, DONE, ERR)
    }
  }

  private def vfs_walk(PATH: path, USER: user, INO: Ref[Int], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    var path: path = PATH
    while (! path.isEmpty && ERR.get == types.error.ESUCCESS) {
      may_lookup(INO.get, USER, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val DENT = new Ref[dentry](types.dentry.negdentry(path.head))
        afs.afs_lookup(INO.get, DENT, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          INO := DENT.get.ino
          path = path.tail
        }
      }
    }
  }

  private def vfs_write_block(START: Int, END: Int, INODE: inode, BUF: buffer, TOTAL: Ref[Int], DONE: Ref[Boolean], ERR: Ref[error]): Unit = {
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

  private def vfs_write_loop(START: Int, END: Int, INODE: inode, BUF: buffer, TOTAL: Ref[Int], DONE: Ref[Boolean], ERR: Ref[error]): Unit = {
    while (ERR.get == types.error.ESUCCESS && DONE.get != true) {
      vfs_write_block(START, END, INODE, BUF, TOTAL, DONE, ERR)
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

object VFS {
  def apply(OF: open_files, afs: AFS)(implicit _algebraic_implicit: algebraic.Algebraic) = {
    new VFS(OF, afs)
  }
}
