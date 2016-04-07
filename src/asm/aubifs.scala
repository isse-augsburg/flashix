// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error

class aubifs_asm(val aubifs_internal_asm : aubifs_internal_asm_interface)(implicit _algebraic_implicit: algebraic.Algebraic) extends logfs_interface {
  import _algebraic_implicit._

  override def afs_check_commit(ERR: Ref[error]): Unit = {
    aubifs_internal_asm.aubifs_internal_check_commit(ERR)
  }

  override def afs_create(P_INODE: inode, MD: metadata, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val KEY3 = new Ref[key](types.key.uninit)
    aubifs_internal_asm.index_newino(KEY3)
    val KEY1: key = types.key.inodekey(P_INODE.ino)
    val KEY2: key = types.key.dentrykey(P_INODE.ino, DENT.get.name)
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    aubifs_internal_asm.index_checkkey(KEY1, EXISTS, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_internal_asm.index_checkkey(KEY2, EXISTS, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_internal_asm.index_checkkey(KEY3.get, EXISTS, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      val ND1: node = types.node.inodenode(KEY1, P_INODE.meta, P_INODE.directory, P_INODE.nlink, P_INODE.nsubdirs, P_INODE.size + 1)
      val ND2: node = types.node.dentrynode(KEY2, KEY3.get.ino)
      val ND3: node = types.node.inodenode(KEY3.get, MD, false, 1, 0, 0)
      val ADR1 = new Ref[address](types.address.uninit)
      val ADR2 = new Ref[address](types.address.uninit)
      val ADR3 = new Ref[address](types.address.uninit)
      aubifs_internal_asm.journal_add3(ND1, ND2, ND3, ADR1, ADR2, ADR3, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        aubifs_internal_asm.index_store(KEY1, ADR1.get, ND1, ERR)
        aubifs_internal_asm.index_store(KEY2, ADR2.get, ND2, ERR)
        aubifs_internal_asm.index_store(KEY3.get, ADR3.get, ND3, ERR)
        DENT := types.dentry.mkdentry(DENT.get.name, KEY3.get.ino)
      } else {
        aubifs_internal_asm.index_remove(KEY2)
        aubifs_internal_asm.index_remove(KEY3.get)
      }
    }
  }

  override def afs_evict(INODE: inode, ERR: Ref[error]): Unit = {
    val KEY: key = types.key.inodekey(INODE.ino)
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    aubifs_internal_asm.orphans_contains(KEY, EXISTS)
    if (EXISTS.get) {
      aubifs_internal_asm.index_checkkey(KEY, EXISTS, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        aubifs_internal_asm.index_checkdata(KEY, 0, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        aubifs_internal_asm.index_truncate(KEY, 0, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        aubifs_internal_asm.orphan_remove(KEY)
        aubifs_internal_asm.index_remove(KEY)
      }
    }
  }

  override def afs_format(VOLSIZE: Int, MD: metadata, ERR: Ref[error]): Unit = {
    aubifs_internal_asm.internal_format(VOLSIZE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val KEY: key = types.key.inodekey(ROOT_INO)
      val ND: node = types.node.inodenode(KEY, MD, true, 0, 0, 0)
      val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
      aubifs_internal_asm.index_checkkey(KEY, EXISTS, ERR)
      val ADR = new Ref[address](types.address.uninit)
      if (ERR.get == types.error.ESUCCESS) {
        aubifs_internal_asm.journal_add1(ND, ADR, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        aubifs_internal_asm.index_store(KEY, ADR.get, ND, ERR)
      }
    }
  }

  override def afs_iget(INO: Int, INODE: inode, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    aubifs_iget_check(INO, EXISTS, INODE, ERR)
  }

  override def afs_link(P_INO: Int, OLD_DENT: dentry, NEW_DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    val P_INODE: inode = types.inode.uninit
    aubifs_pget_check(P_INO, EXISTS, P_INODE, ERR)
    val INO: Int = OLD_DENT.ino
    val INODE: inode = types.inode.uninit
    aubifs_iget_check(INO, EXISTS, INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val KEY1: key = types.key.inodekey(P_INO)
      val KEY2: key = types.key.dentrykey(P_INO, NEW_DENT.get.name)
      aubifs_internal_asm.index_checkkey(KEY2, EXISTS, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val KEY3: key = types.key.inodekey(INO)
        val ND1: node = types.node.inodenode(KEY1, P_INODE.meta, P_INODE.directory, P_INODE.nlink, P_INODE.nsubdirs, P_INODE.size + 1)
        val ND2: node = types.node.dentrynode(KEY2, INO)
        val ND3: node = types.node.inodenode(KEY3, INODE.meta, INODE.directory, INODE.nlink + 1, INODE.nsubdirs, INODE.size)
        val ADR1 = new Ref[address](types.address.uninit)
        val ADR2 = new Ref[address](types.address.uninit)
        val ADR3 = new Ref[address](types.address.uninit)
        aubifs_internal_asm.journal_add3(ND1, ND2, ND3, ADR1, ADR2, ADR3, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          aubifs_internal_asm.index_store(KEY1, ADR1.get, ND1, ERR)
          aubifs_internal_asm.index_store(KEY2, ADR2.get, ND2, ERR)
          aubifs_internal_asm.index_store(KEY3, ADR3.get, ND3, ERR)
          NEW_DENT := types.dentry.mkdentry(NEW_DENT.get.name, INO)
        } else {
          aubifs_internal_asm.index_remove(KEY2)
        }
      }
    }
  }

  override def afs_lookup(P_INO: Int, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val KEY: key = types.key.dentrykey(P_INO, DENT.get.name)
    val ND = new Ref[node](types.node.uninit)
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    aubifs_internal_asm.index_lookup(KEY, EXISTS, ND, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (EXISTS.get) {
        ERR := types.error.ESUCCESS
        DENT := types.dentry.mkdentry(DENT.get.name, ND.get.ino)
      } else {
        ERR := types.error.ENOENT
        DENT := types.dentry.negdentry(DENT.get.name)
      }
    }
  }

  override def afs_mkdir(P_INODE: inode, MD: metadata, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val KEY3 = new Ref[key](types.key.uninit)
    aubifs_internal_asm.index_newino(KEY3)
    val KEY1: key = types.key.inodekey(P_INODE.ino)
    val KEY2: key = types.key.dentrykey(P_INODE.ino, DENT.get.name)
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    aubifs_internal_asm.index_checkkey(KEY1, EXISTS, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_internal_asm.index_checkkey(KEY2, EXISTS, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_internal_asm.index_checkkey(KEY3.get, EXISTS, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      val ND1: node = types.node.inodenode(KEY1, P_INODE.meta, P_INODE.directory, P_INODE.nlink, P_INODE.nsubdirs + 1, P_INODE.size + 1)
      val ND2: node = types.node.dentrynode(KEY2, KEY3.get.ino)
      val ND3: node = types.node.inodenode(KEY3.get, MD, true, 1, 0, 0)
      val ADR1 = new Ref[address](types.address.uninit)
      val ADR2 = new Ref[address](types.address.uninit)
      val ADR3 = new Ref[address](types.address.uninit)
      aubifs_internal_asm.journal_add3(ND1, ND2, ND3, ADR1, ADR2, ADR3, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        aubifs_internal_asm.index_store(KEY1, ADR1.get, ND1, ERR)
        aubifs_internal_asm.index_store(KEY2, ADR2.get, ND2, ERR)
        aubifs_internal_asm.index_store(KEY3.get, ADR3.get, ND3, ERR)
        DENT := types.dentry.mkdentry(DENT.get.name, KEY3.get.ino)
      } else {
        aubifs_internal_asm.index_remove(KEY2)
        aubifs_internal_asm.index_remove(KEY3.get)
      }
    }
  }

  override def afs_readdir(INODE: inode, NAMES: stringset, ERR: Ref[error]): Unit = {
    val KEY: key = types.key.inodekey(INODE.ino)
    aubifs_internal_asm.index_entries(KEY, NAMES, ERR)
  }

  override def afs_readpage(INODE: inode, PAGENO: Int, PBUF: buffer, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val ND = new Ref[node](types.node.uninit)
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    val KEY: key = types.key.datakey(INODE.ino, PAGENO)
    aubifs_internal_asm.index_lookup(KEY, EXISTS, ND, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (EXISTS.get)
        PBUF := ND.get.data.deepCopy
      else
        PBUF := zeropage
    }
  }

  override def afs_recovery(ERR: Ref[error]): Unit = {
    val AX: address_list = new address_list()
    val KS: key_set = new key_set()
    aubifs_internal_asm.aubifs_readflash(AX, KS, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_replayorphans(KS, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_replaylog(AX, ERR)
    }
  }

  override def afs_rename(OLD_INO: Int, NEW_INO: Int, OLD_DENT: Ref[dentry], NEW_DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    var ND5: node = types.node.uninit
    val ADR5 = new Ref[address](types.address.uninit)
    var KEY5: key = types.key.uninit
    val OLD_INODE: inode = types.inode.uninit
    val ADR4 = new Ref[address](types.address.uninit)
    var ND3: node = types.node.uninit
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    var KEY2: key = types.key.uninit
    var KEY4: key = types.key.uninit
    val ADR1 = new Ref[address](types.address.uninit)
    var IS_DIR: Boolean = helpers.scala.Boolean.uninit
    val INODE: inode = types.inode.uninit
    var KEY3: key = types.key.uninit
    var ND1: node = types.node.uninit
    var ND2: node = types.node.uninit
    var KEY1: key = types.key.uninit
    val ADR2 = new Ref[address](types.address.uninit)
    var ND4: node = types.node.uninit
    val ADR3 = new Ref[address](types.address.uninit)
    val NEW_INODE: inode = types.inode.uninit
    val REPARENT: Boolean = OLD_INO != NEW_INO
    val IDENTITY: Boolean = OLD_DENT.get == NEW_DENT.get
    val OVERWRITE: Boolean = NEW_DENT.get.isInstanceOf[types.dentry.mkdentry]
    if (OVERWRITE || REPARENT) {
      val FD: Int = OLD_INO
      aubifs_iget_check(FD, EXISTS, INODE, ERR)
      OLD_INODE := INODE.deepCopy
      val INO: Int = OLD_DENT.get.ino
      aubifs_iget_check(INO, EXISTS, INODE, ERR)
      IS_DIR = INODE.directory
    }
    if (REPARENT) {
      val INO: Int = NEW_INO
      aubifs_iget_check(INO, EXISTS, INODE, ERR)
      NEW_INODE := INODE.deepCopy
    }
    if (OVERWRITE) {
      val INO: Int = NEW_DENT.get.ino
      aubifs_iget_check(INO, EXISTS, INODE, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      KEY1 = types.key.dentrykey(OLD_INO, OLD_DENT.get.name)
      KEY2 = types.key.dentrykey(NEW_INO, NEW_DENT.get.name)
      ND1 = types.node.dentrynode(KEY1, 0)
      ND2 = types.node.dentrynode(KEY2, OLD_DENT.get.ino)
      if (REPARENT || OVERWRITE) {
        KEY3 = types.key.inodekey(OLD_INO)
        ND3 = types.node.inodenode(KEY3, OLD_INODE.meta, OLD_INODE.directory, OLD_INODE.nlink, OLD_INODE.nsubdirs - (if (IS_DIR) 1 else 0), OLD_INODE.size - 1)
      }
      if (OVERWRITE != true && REPARENT) {
        KEY4 = types.key.inodekey(NEW_INO)
        ND4 = types.node.inodenode(KEY4, NEW_INODE.meta, NEW_INODE.directory, NEW_INODE.nlink, NEW_INODE.nsubdirs + (if (IS_DIR) 1 else 0), NEW_INODE.size + 1)
      }
      if (OVERWRITE && (IDENTITY != true || REPARENT)) {
        KEY5 = types.key.inodekey(NEW_DENT.get.ino)
        ND5 = types.node.inodenode(KEY5, INODE.meta, INODE.directory, INODE.nlink - 1, INODE.nsubdirs, INODE.size)
      }
      aubifs_internal_asm.index_checkkey(KEY2, EXISTS, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      if (OVERWRITE != true && REPARENT) {
        aubifs_internal_asm.journal_add4(ND1, ND2, ND3, ND4, ADR1, ADR2, ADR3, ADR4, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          aubifs_internal_asm.index_remove(KEY1)
          aubifs_internal_asm.index_store(KEY2, ADR2.get, ND2, ERR)
          aubifs_internal_asm.index_store(KEY3, ADR3.get, ND3, ERR)
          aubifs_internal_asm.index_store(KEY4, ADR4.get, ND4, ERR)
        }
      }
      if (OVERWRITE) {
        aubifs_internal_asm.journal_add4(ND1, ND2, ND3, ND5, ADR1, ADR2, ADR3, ADR5, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          aubifs_internal_asm.index_remove(KEY1)
          aubifs_internal_asm.index_store(KEY2, ADR2.get, ND2, ERR)
          aubifs_internal_asm.index_store(KEY3, ADR3.get, ND3, ERR)
          aubifs_internal_asm.index_store(KEY5, ADR5.get, ND5, ERR)
        }
      }
      if (OVERWRITE != true && REPARENT != true) {
        aubifs_internal_asm.journal_add2(ND1, ND2, ADR1, ADR2, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          aubifs_internal_asm.index_remove(KEY1)
          aubifs_internal_asm.index_store(KEY2, ADR2.get, ND2, ERR)
        }
      }
    }
    if (ERR.get == types.error.ESUCCESS) {
      if (INODE.nlink == 1) {
        aubifs_internal_asm.orphan_insert(KEY5)
      }
      NEW_DENT := types.dentry.mkdentry(NEW_DENT.get.name, OLD_DENT.get.ino)
      OLD_DENT := types.dentry.negdentry(OLD_DENT.get.name)
    }
  }

  override def afs_rmdir(P_INODE: inode, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val INO: Int = DENT.get.ino
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    val INODE: inode = types.inode.uninit
    aubifs_iget_check(INO, EXISTS, INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val KEY1: key = types.key.inodekey(P_INODE.ino)
      aubifs_internal_asm.index_checkkey(KEY1, EXISTS, ERR)
      val KEY2: key = types.key.dentrykey(P_INODE.ino, DENT.get.name)
      if (ERR.get == types.error.ESUCCESS) {
        aubifs_internal_asm.index_checkkey(KEY2, EXISTS, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        val KEY3: key = types.key.inodekey(INODE.ino)
        val ND1: node = types.node.inodenode(KEY1, P_INODE.meta, P_INODE.directory, P_INODE.nlink, P_INODE.nsubdirs - 1, P_INODE.size - 1)
        val ND2: node = types.node.dentrynode(KEY2, 0)
        val ND3: node = types.node.inodenode(KEY3, INODE.meta, INODE.directory, INODE.nlink - 1, INODE.nsubdirs, INODE.size)
        val ADR1 = new Ref[address](types.address.uninit)
        val ADR3 = new Ref[address](types.address.uninit)
        val ADR2 = new Ref[address](types.address.uninit)
        aubifs_internal_asm.journal_add3(ND1, ND2, ND3, ADR1, ADR2, ADR3, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          aubifs_internal_asm.index_store(KEY1, ADR1.get, ND1, ERR)
          aubifs_internal_asm.index_remove(KEY2)
          aubifs_internal_asm.index_store(KEY3, ADR3.get, ND3, ERR)
          aubifs_internal_asm.orphan_insert(KEY3)
          DENT := types.dentry.negdentry(DENT.get.name)
        }
      }
    }
  }

  override def afs_truncate(INODE: inode, N: Int, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val ND3 = new Ref[node](types.node.uninit)
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    var KEY2: key = types.key.uninit
    val ADR1 = new Ref[address](types.address.uninit)
    var KEY3: key = types.key.uninit
    var ND1: node = types.node.uninit
    var ND2: node = types.node.uninit
    val PBUF: buffer = new buffer()
    var KEY1: key = types.key.uninit
    val ADR2 = new Ref[address](types.address.uninit)
    val ADR3 = new Ref[address](types.address.uninit)
    val PAGENO: Int = INODE.size / VFS_PAGE_SIZE
    val OFFSET: Int = INODE.size % VFS_PAGE_SIZE
    val SIZE: Int = min(N, INODE.size)
    KEY1 = types.key.inodekey(INODE.ino)
    KEY2 = KEY1
    KEY3 = types.key.datakey(INODE.ino, PAGENO)
    ND1 = types.node.truncnode(KEY1, SIZE)
    ND2 = types.node.inodenode(KEY2, INODE.meta, INODE.directory, INODE.nlink, INODE.nsubdirs, N)
    aubifs_internal_asm.index_checkkey(KEY2, EXISTS, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_internal_asm.index_checkdata(KEY1, SIZE, ERR)
    }
    if (ERR.get == types.error.ESUCCESS && (INODE.size <= N && OFFSET != 0)) {
      aubifs_internal_asm.index_lookup(KEY3, EXISTS, ND3, ERR)
    } else
      EXISTS := false
    if (ERR.get == types.error.ESUCCESS && EXISTS.get)
      PBUF := ND3.get.data.deepCopy
    
    if (ERR.get == types.error.ESUCCESS) {
      if (EXISTS.get) {
        PBUF.fill(zero, OFFSET, VFS_PAGE_SIZE - OFFSET)
        ND3 := types.node.datanode(KEY3, PBUF).deepCopy
        aubifs_internal_asm.journal_add3(ND1, ND2, ND3.get, ADR1, ADR2, ADR3, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          aubifs_internal_asm.index_store(KEY3, ADR3.get, ND3.get, ERR)
        }
      } else {
        aubifs_internal_asm.journal_add2(ND1, ND2, ADR1, ADR2, ERR)
      }
    }
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_internal_asm.index_store(KEY2, ADR2.get, ND2, ERR)
      aubifs_internal_asm.index_truncate(KEY1, SIZE, ERR)
    }
  }

  override def afs_unlink(P_INODE: inode, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val INO: Int = DENT.get.ino
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    val INODE: inode = types.inode.uninit
    aubifs_iget_check(INO, EXISTS, INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val KEY1: key = types.key.inodekey(P_INODE.ino)
      aubifs_internal_asm.index_checkkey(KEY1, EXISTS, ERR)
      val KEY2: key = types.key.dentrykey(P_INODE.ino, DENT.get.name)
      if (ERR.get == types.error.ESUCCESS) {
        aubifs_internal_asm.index_checkkey(KEY2, EXISTS, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        val KEY3: key = types.key.inodekey(INODE.ino)
        val ND1: node = types.node.inodenode(KEY1, P_INODE.meta, P_INODE.directory, P_INODE.nlink, P_INODE.nsubdirs, P_INODE.size - 1)
        val ND2: node = types.node.dentrynode(KEY2, 0)
        val ND3: node = types.node.inodenode(KEY3, INODE.meta, INODE.directory, INODE.nlink - 1, INODE.nsubdirs, INODE.size)
        val ADR1 = new Ref[address](types.address.uninit)
        val ADR3 = new Ref[address](types.address.uninit)
        val ADR2 = new Ref[address](types.address.uninit)
        aubifs_internal_asm.journal_add3(ND1, ND2, ND3, ADR1, ADR2, ADR3, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          aubifs_internal_asm.index_store(KEY1, ADR1.get, ND1, ERR)
          aubifs_internal_asm.index_remove(KEY2)
          aubifs_internal_asm.index_store(KEY3, ADR3.get, ND3, ERR)
          if (INODE.nlink == 1) {
            aubifs_internal_asm.orphan_insert(KEY3)
          }
          DENT := types.dentry.negdentry(DENT.get.name)
        }
      }
    }
  }

  override def afs_write_inode(INODE: inode, ERR: Ref[error]): Unit = {
    val KEY: key = types.key.inodekey(INODE.ino)
    val ND: node = types.node.inodenode(KEY, INODE.meta, INODE.directory, INODE.nlink, INODE.nsubdirs, INODE.size)
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    aubifs_internal_asm.index_checkkey(KEY, EXISTS, ERR)
    val ADR = new Ref[address](types.address.uninit)
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_internal_asm.journal_add1(ND, ADR, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_internal_asm.index_store(KEY, ADR.get, ND, ERR)
    }
  }

  override def afs_writepage(INODE: inode, PAGENO: Int, PBUF: buffer, ERR: Ref[error]): Unit = {
    var KEY1: key = types.key.uninit
    val INO: Int = INODE.ino
    KEY1 = types.key.datakey(INO, PAGENO)
    val ND1: node = types.node.datanode(KEY1, PBUF).deepCopy
    val EXISTS = new Ref[Boolean](true)
    aubifs_internal_asm.index_checkkey(KEY1, EXISTS, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val ADR1 = new Ref[address](types.address.uninit)
      aubifs_internal_asm.journal_add1(ND1, ADR1, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        aubifs_internal_asm.index_store(KEY1, ADR1.get, ND1, ERR)
      } else       if (EXISTS.get != true) {
        aubifs_internal_asm.index_remove(KEY1)
      }
    }
  }

  private def aubifs_iget_check(INO: Int, EXISTS: Ref[Boolean], INODE: inode, ERR: Ref[error]): Unit = {
    if (ERR.get == types.error.ESUCCESS) {
      val KEY: key = types.key.inodekey(INO)
      val ND = new Ref[node](types.node.uninit)
      aubifs_internal_asm.index_lookup(KEY, EXISTS, ND, ERR)
      if (ERR.get == types.error.ESUCCESS && EXISTS.get)
        INODE := types.inode.mkinode(INO, ND.get.meta, ND.get.directory, ND.get.nlink, if (ND.get.directory) ND.get.nsubdirs else 0, ND.get.size)
      
    }
  }

  private def aubifs_pget_check(P_INO: Int, EXISTS: Ref[Boolean], P_INODE: inode, ERR: Ref[error]): Unit = {
    val INODE: inode = types.inode.uninit
    val INO: Int = P_INO
    aubifs_iget_check(INO, EXISTS, INODE, ERR)
    P_INODE := INODE
  }

  private def aubifs_replaylog(LOG0: address_list, ERR: Ref[error]): Unit = {
    val AX: address_list = LOG0.deepCopy
    while (ERR.get == types.error.ESUCCESS && ! AX.isEmpty) {
      val ADR: address = AX.head
      aubifs_replayone(ADR, ERR)
      AX.removeHead
    }
  }

  private def aubifs_replayone(ADR: address, ERR: Ref[error]): Unit = {
    val ND = new Ref[node](types.node.uninit)
    aubifs_internal_asm.journal_get(ADR, ND, ERR)
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_internal_asm.index_checkkey(ND.get.key, EXISTS, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      if (ND.get.isInstanceOf[types.node.inodenode]) {
        if (ND.get.nlink != 0 || ND.get.key.ino == ROOT_INO) {
          aubifs_internal_asm.index_store(ND.get.key, ADR, ND.get, ERR)
        } else {
          if (! ND.get.directory) {
            aubifs_internal_asm.index_checkdata(ND.get.key, 0, ERR)
            if (ERR.get == types.error.ESUCCESS) {
              aubifs_internal_asm.index_truncate(ND.get.key, 0, ERR)
            }
          }
          if (ERR.get == types.error.ESUCCESS) {
            aubifs_internal_asm.index_remove(ND.get.key)
          }
        }
      }
      if (ND.get.isInstanceOf[types.node.dentrynode]) {
        if (ND.get.ino != 0) {
          aubifs_internal_asm.index_store(ND.get.key, ADR, ND.get, ERR)
        } else {
          aubifs_internal_asm.index_remove(ND.get.key)
        }
      }
      if (ND.get.isInstanceOf[types.node.datanode]) {
        val KEY: key = types.key.inodekey(ND.get.key.ino)
        aubifs_internal_asm.index_contains(KEY, EXISTS, ERR)
        if (ERR.get == types.error.ESUCCESS && EXISTS.get) {
          aubifs_internal_asm.index_store(ND.get.key, ADR, ND.get, ERR)
        }
      }
      if (ND.get.isInstanceOf[types.node.truncnode]) {
        aubifs_internal_asm.index_checkdata(ND.get.key, ND.get.size, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          aubifs_internal_asm.index_truncate(ND.get.key, ND.get.size, ERR)
        }
      }
    }
  }

  private def aubifs_replayorphans(KS: key_set, ERR: Ref[error]): Unit = {
    while (! KS.isEmpty && ERR.get == types.error.ESUCCESS) {
      val KEY: key = KS.head
      aubifs_internal_asm.index_checkdata(KEY, 0, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        aubifs_internal_asm.index_truncate(KEY, 0, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        aubifs_internal_asm.index_remove(KEY)
      }
      if (ERR.get == types.error.ESUCCESS) {
        aubifs_internal_asm.orphan_remove(KEY)
        KS -= KEY
      }
    }
  }

  override def logfs_gc(): Unit = {
    aubifs_internal_asm.journal_gc()
  }

}

