package asm

import helpers.scala._
import types._
import types.error.error

class AUBIFS(val journal : AUBIFSJournal)(implicit _algebraic_implicit: algebraic.Algebraic) extends LogFS {
  import _algebraic_implicit._

  override def afs_check_commit(ERR: Ref[error]): Unit = {
    journal.aubifs_internal_check_commit(ERR)
  }

  override def afs_create(P_INO: Int, MD: metadata, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    val P_INODE = new Ref[inode](inode.uninit)
    aubifs_pget_check(P_INO, EXISTS, P_INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val KEY3 = new Ref[key](key.uninit)
      journal.index_newino(KEY3, ERR)
      val KEY2: key = types.key.dentrykey(P_INO, DENT.get.name)
      if (ERR.get == types.error.ESUCCESS) {
        journal.index_checkkey(KEY2, EXISTS, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        journal.index_checkkey(KEY3.get, EXISTS, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        val KEY1: key = types.key.inodekey(P_INODE.get.ino)
        val ND1: node = types.node.inodenode(KEY1, P_INODE.get.meta, P_INODE.get.directory, P_INODE.get.nlink, P_INODE.get.size + 1)
        val ND2: node = types.node.dentrynode(KEY2, KEY3.get.ino)
        val ND3: node = types.node.inodenode(KEY3.get, MD, false, 1, 0)
        val ADR3 = new Ref[address](address.uninit)
        val ADR1 = new Ref[address](address.uninit)
        val ADR2 = new Ref[address](address.uninit)
        journal.journal_add3(ND1, ND2, ND3, ADR1, ADR2, ADR3, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          journal.index_store(KEY1, ADR1.get, ND1, ERR)
          journal.index_store(KEY2, ADR2.get, ND2, ERR)
          journal.index_store(KEY3.get, ADR3.get, ND3, ERR)
          DENT := types.dentry.mkdentry(DENT.get.name, KEY3.get.ino)
        } else {
          journal.index_remove(KEY2)
          journal.index_remove(KEY3.get)
        }
      }
    }
  }

  override def afs_evict(INO: Int, ERR: Ref[error]): Unit = {
    val KEY: key = types.key.inodekey(INO)
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    journal.orphans_contains(KEY, EXISTS)
    if (EXISTS.get) {
      journal.index_checkkey(KEY, EXISTS, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        journal.index_checkdata(KEY, 0, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        journal.index_truncate(KEY, 0, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        journal.orphan_remove(KEY)
        journal.index_remove(KEY)
      }
    }
  }

  override def afs_format(VOLSIZE: Int, MD: metadata, ERR: Ref[error]): Unit = {
    journal.aubifs_internal_format(VOLSIZE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val KEY: key = types.key.inodekey(ROOT_INO)
      val ND: node = types.node.inodenode(KEY, MD, true, 0, 0)
      val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
      journal.index_checkkey(KEY, EXISTS, ERR)
      val ADR = new Ref[address](address.uninit)
      if (ERR.get == types.error.ESUCCESS) {
        journal.journal_add1(ND, ADR, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        journal.index_store(KEY, ADR.get, ND, ERR)
      }
    }
  }

  override def afs_iget(INO: Int, INODE: Ref[inode], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    aubifs_iget_check(INO, EXISTS, INODE, ERR)
  }

  override def afs_link(P_INO: Int, OLD_DENT: dentry, NEW_DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    val P_INODE = new Ref[inode](inode.uninit)
    aubifs_pget_check(P_INO, EXISTS, P_INODE, ERR)
    val INO: Int = OLD_DENT.ino
    val INODE = new Ref[inode](inode.uninit)
    aubifs_iget_check(INO, EXISTS, INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val KEY1: key = types.key.inodekey(P_INO)
      val KEY2: key = types.key.dentrykey(P_INO, NEW_DENT.get.name)
      journal.index_checkkey(KEY2, EXISTS, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val KEY3: key = types.key.inodekey(INO)
        val ND1: node = types.node.inodenode(KEY1, P_INODE.get.meta, P_INODE.get.directory, P_INODE.get.nlink, P_INODE.get.size + 1)
        val ND2: node = types.node.dentrynode(KEY2, INO)
        val ND3: node = types.node.inodenode(KEY3, INODE.get.meta, INODE.get.directory, INODE.get.nlink + 1, INODE.get.size)
        val ADR3 = new Ref[address](address.uninit)
        val ADR1 = new Ref[address](address.uninit)
        val ADR2 = new Ref[address](address.uninit)
        journal.journal_add3(ND1, ND2, ND3, ADR1, ADR2, ADR3, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          journal.index_store(KEY1, ADR1.get, ND1, ERR)
          journal.index_store(KEY2, ADR2.get, ND2, ERR)
          journal.index_store(KEY3, ADR3.get, ND3, ERR)
          NEW_DENT := types.dentry.mkdentry(NEW_DENT.get.name, INO)
        } else {
          journal.index_remove(KEY2)
        }
      }
    }
  }

  override def afs_lookup(P_INO: Int, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val KEY: key = types.key.dentrykey(P_INO, DENT.get.name)
    val ND = new Ref[node](node.uninit)
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    journal.index_lookup(KEY, EXISTS, ND, ERR)
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

  override def afs_mkdir(P_INO: Int, MD: metadata, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    val P_INODE = new Ref[inode](inode.uninit)
    aubifs_pget_check(P_INO, EXISTS, P_INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val KEY3 = new Ref[key](key.uninit)
      journal.index_newino(KEY3, ERR)
      val KEY2: key = types.key.dentrykey(P_INO, DENT.get.name)
      if (ERR.get == types.error.ESUCCESS) {
        journal.index_checkkey(KEY2, EXISTS, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        journal.index_checkkey(KEY3.get, EXISTS, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        val KEY1: key = types.key.inodekey(P_INODE.get.ino)
        val ND1: node = types.node.inodenode(KEY1, P_INODE.get.meta, P_INODE.get.directory, P_INODE.get.nlink, P_INODE.get.size + 1)
        val ND2: node = types.node.dentrynode(KEY2, KEY3.get.ino)
        val ND3: node = types.node.inodenode(KEY3.get, MD, true, 1, 0)
        val ADR3 = new Ref[address](address.uninit)
        val ADR1 = new Ref[address](address.uninit)
        val ADR2 = new Ref[address](address.uninit)
        journal.journal_add3(ND1, ND2, ND3, ADR1, ADR2, ADR3, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          journal.index_store(KEY1, ADR1.get, ND1, ERR)
          journal.index_store(KEY2, ADR2.get, ND2, ERR)
          journal.index_store(KEY3.get, ADR3.get, ND3, ERR)
          DENT := types.dentry.mkdentry(DENT.get.name, KEY3.get.ino)
        } else {
          journal.index_remove(KEY2)
          journal.index_remove(KEY3.get)
        }
      }
    }
  }

  override def afs_readdir(INO: Int, NAMES: stringset, ERR: Ref[error]): Unit = {
    val KEY: key = types.key.inodekey(INO)
    journal.index_entries(KEY, NAMES, ERR)
  }

  override def afs_readpage(INODE: inode, PAGENO: Int, PBUF: buffer, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val ND = new Ref[node](node.uninit)
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    val KEY: key = types.key.datakey(INODE.ino, PAGENO)
    journal.index_lookup(KEY, EXISTS, ND, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (EXISTS.get)
        PBUF := ND.get.data.deepCopy
      else
        PBUF := zeropage
    }
  }

  override def afs_rename(OLD_INO: Int, NEW_INO: Int, OLD_DENT: Ref[dentry], NEW_DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    var KEY5: key = key.uninit
    var KEY3: key = key.uninit
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    var KEY4: key = key.uninit
    var ND3: node = node.uninit
    var IS_DIR: Boolean = helpers.scala.Boolean.uninit
    var KEY2: key = key.uninit
    val ADR3 = new Ref[address](address.uninit)
    val INODE = new Ref[inode](inode.uninit)
    val ADR5 = new Ref[address](address.uninit)
    val ADR1 = new Ref[address](address.uninit)
    var ND2: node = node.uninit
    var NEW_INODE: inode = inode.uninit
    val ADR2 = new Ref[address](address.uninit)
    var ND1: node = node.uninit
    var OLD_INODE: inode = inode.uninit
    var ND5: node = node.uninit
    var ND4: node = node.uninit
    var KEY1: key = key.uninit
    val ADR4 = new Ref[address](address.uninit)
    val OVERWRITE: Boolean = NEW_DENT.get.isInstanceOf[types.dentry.mkdentry]
    val IDENTITY: Boolean = OLD_DENT.get == NEW_DENT.get
    val REPARENT: Boolean = OLD_INO != NEW_INO
    if (OVERWRITE || REPARENT) {
      val INO0: Int = OLD_INO
      aubifs_iget_check(INO0, EXISTS, INODE, ERR)
      OLD_INODE = INODE.get
      val INO: Int = OLD_DENT.get.ino
      aubifs_iget_check(INO, EXISTS, INODE, ERR)
      IS_DIR = INODE.get.directory
    }
    if (REPARENT) {
      val INO: Int = NEW_INO
      aubifs_iget_check(INO, EXISTS, INODE, ERR)
      NEW_INODE = INODE.get
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
        ND3 = types.node.inodenode(KEY3, OLD_INODE.meta, OLD_INODE.directory, OLD_INODE.nlink, OLD_INODE.size - 1)
      }
      if (OVERWRITE != true && REPARENT) {
        KEY4 = types.key.inodekey(NEW_INO)
        ND4 = types.node.inodenode(KEY4, NEW_INODE.meta, NEW_INODE.directory, NEW_INODE.nlink, NEW_INODE.size + 1)
      }
      if (OVERWRITE && (IDENTITY != true || REPARENT)) {
        KEY5 = types.key.inodekey(NEW_DENT.get.ino)
        ND5 = types.node.inodenode(KEY5, INODE.get.meta, INODE.get.directory, INODE.get.nlink - 1, INODE.get.size)
      }
      journal.index_checkkey(KEY2, EXISTS, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      if (OVERWRITE != true && REPARENT) {
        journal.journal_add4(ND1, ND2, ND3, ND4, ADR1, ADR2, ADR3, ADR4, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          journal.index_remove(KEY1)
          journal.index_store(KEY2, ADR2.get, ND2, ERR)
          journal.index_store(KEY3, ADR3.get, ND3, ERR)
          journal.index_store(KEY4, ADR4.get, ND4, ERR)
        }
      }
      if (OVERWRITE) {
        journal.journal_add4(ND1, ND2, ND3, ND5, ADR1, ADR2, ADR3, ADR5, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          journal.index_remove(KEY1)
          journal.index_store(KEY2, ADR2.get, ND2, ERR)
          journal.index_store(KEY3, ADR3.get, ND3, ERR)
          journal.index_store(KEY5, ADR5.get, ND5, ERR)
        }
      }
      if (OVERWRITE != true && REPARENT != true) {
        journal.journal_add2(ND1, ND2, ADR1, ADR2, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          journal.index_remove(KEY1)
          journal.index_store(KEY2, ADR2.get, ND2, ERR)
        }
      }
    }
    if (ERR.get == types.error.ESUCCESS) {
      if (INODE.get.nlink == 1) {
        journal.orphan_insert(KEY5)
      }
      NEW_DENT := types.dentry.mkdentry(NEW_DENT.get.name, OLD_DENT.get.ino)
      OLD_DENT := types.dentry.negdentry(OLD_DENT.get.name)
    }
  }

  override def afs_rmdir(P_INO: Int, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    val P_INODE = new Ref[inode](inode.uninit)
    aubifs_pget_check(P_INO, EXISTS, P_INODE, ERR)
    val INO: Int = DENT.get.ino
    val INODE = new Ref[inode](inode.uninit)
    aubifs_iget_check(INO, EXISTS, INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val KEY2: key = types.key.dentrykey(P_INO, DENT.get.name)
      journal.index_checkkey(KEY2, EXISTS, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val KEY3: key = types.key.inodekey(INODE.get.ino)
        var ND3: node = node.uninit
        ND3 = types.node.inodenode(KEY3, INODE.get.meta, INODE.get.directory, INODE.get.nlink - 1, INODE.get.size)
        val KEY1: key = types.key.inodekey(P_INODE.get.ino)
        val ND1: node = types.node.inodenode(KEY1, P_INODE.get.meta, P_INODE.get.directory, P_INODE.get.nlink, P_INODE.get.size - 1)
        val ADR3 = new Ref[address](address.uninit)
        val ADR1 = new Ref[address](address.uninit)
        val ND2: node = types.node.dentrynode(KEY2, 0)
        val ADR2 = new Ref[address](address.uninit)
        journal.journal_add3(ND1, ND2, ND3, ADR1, ADR2, ADR3, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          journal.index_store(KEY1, ADR1.get, ND1, ERR)
          journal.index_remove(KEY2)
          journal.index_store(KEY3, ADR3.get, ND3, ERR)
          journal.orphan_insert(KEY3)
          DENT := types.dentry.negdentry(DENT.get.name)
        }
      }
    }
  }

  override def afs_truncate(INODE: inode, N: Int, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    var KEY3: key = key.uninit
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    val ND3 = new Ref[node](node.uninit)
    var KEY2: key = key.uninit
    val ADR3 = new Ref[address](address.uninit)
    val PBUF: buffer = new buffer()
    val ADR1 = new Ref[address](address.uninit)
    var ND2: node = node.uninit
    val ADR2 = new Ref[address](address.uninit)
    var ND1: node = node.uninit
    var KEY1: key = key.uninit
    val SIZE: Int = min(N, INODE.size)
    val PAGENO: Int = INODE.size / VFS_PAGE_SIZE
    val OFFSET: Int = INODE.size % VFS_PAGE_SIZE
    KEY1 = types.key.inodekey(INODE.ino)
    KEY2 = KEY1
    KEY3 = types.key.datakey(INODE.ino, PAGENO)
    ND1 = types.node.truncnode(KEY1, SIZE)
    ND2 = types.node.inodenode(KEY2, INODE.meta, INODE.directory, INODE.nlink, N)
    journal.index_checkkey(KEY2, EXISTS, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      journal.index_checkdata(KEY1, SIZE, ERR)
    }
    if (ERR.get == types.error.ESUCCESS && (INODE.size <= N && OFFSET != 0)) {
      journal.index_lookup(KEY3, EXISTS, ND3, ERR)
    } else
      EXISTS := false
    if (ERR.get == types.error.ESUCCESS && INODE.size <= N) {
      journal.index_truncate(KEY1, SIZE, ERR)
    }
    if (ERR.get == types.error.ESUCCESS && EXISTS.get)
      PBUF := ND3.get.data.deepCopy
    
    if (ERR.get == types.error.ESUCCESS) {
      if (EXISTS.get) {
        PBUF.fill(zero, OFFSET, VFS_PAGE_SIZE - OFFSET)
        ND3 := types.node.datanode(KEY3, PBUF).deepCopy
        journal.journal_add3(ND1, ND2, ND3.get, ADR1, ADR2, ADR3, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          journal.index_store(KEY3, ADR3.get, ND3.get, ERR)
        }
      } else {
        journal.journal_add2(ND1, ND2, ADR1, ADR2, ERR)
      }
    }
    if (ERR.get == types.error.ESUCCESS) {
      journal.index_store(KEY2, ADR2.get, ND2, ERR)
      if (N < INODE.size) {
        journal.index_truncate(KEY1, SIZE, ERR)
      }
    }
  }

  override def afs_unlink(P_INO: Int, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    val P_INODE = new Ref[inode](inode.uninit)
    aubifs_pget_check(P_INO, EXISTS, P_INODE, ERR)
    val INO: Int = DENT.get.ino
    val INODE = new Ref[inode](inode.uninit)
    aubifs_iget_check(INO, EXISTS, INODE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val KEY2: key = types.key.dentrykey(P_INO, DENT.get.name)
      journal.index_checkkey(KEY2, EXISTS, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val KEY3: key = types.key.inodekey(INODE.get.ino)
        var ND3: node = node.uninit
        ND3 = types.node.inodenode(KEY3, INODE.get.meta, INODE.get.directory, INODE.get.nlink - 1, INODE.get.size)
        val KEY1: key = types.key.inodekey(P_INODE.get.ino)
        val ND1: node = types.node.inodenode(KEY1, P_INODE.get.meta, P_INODE.get.directory, P_INODE.get.nlink, P_INODE.get.size - 1)
        val ADR3 = new Ref[address](address.uninit)
        val ADR1 = new Ref[address](address.uninit)
        val ND2: node = types.node.dentrynode(KEY2, 0)
        val ADR2 = new Ref[address](address.uninit)
        journal.journal_add3(ND1, ND2, ND3, ADR1, ADR2, ADR3, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          journal.index_store(KEY1, ADR1.get, ND1, ERR)
          journal.index_remove(KEY2)
          journal.index_store(KEY3, ADR3.get, ND3, ERR)
          if (INODE.get.nlink == 1) {
            journal.orphan_insert(KEY3)
          }
          DENT := types.dentry.negdentry(DENT.get.name)
        }
      }
    }
  }

  override def afs_write_inode(INODE: inode, ERR: Ref[error]): Unit = {
    val KEY: key = types.key.inodekey(INODE.ino)
    val ND: node = types.node.inodenode(KEY, INODE.meta, INODE.directory, INODE.nlink, INODE.size)
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    journal.index_checkkey(KEY, EXISTS, ERR)
    val ADR = new Ref[address](address.uninit)
    if (ERR.get == types.error.ESUCCESS) {
      journal.journal_add1(ND, ADR, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      journal.index_store(KEY, ADR.get, ND, ERR)
    }
  }

  override def afs_writepage(INODE: inode, PAGENO: Int, PBUF: buffer, ERR: Ref[error]): Unit = {
    var KEY1: key = key.uninit
    val INO: Int = INODE.ino
    KEY1 = types.key.datakey(INO, PAGENO)
    val ND1: node = types.node.datanode(KEY1, PBUF).deepCopy
    val EXISTS = new Ref[Boolean](true)
    journal.index_checkkey(KEY1, EXISTS, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val ADR1 = new Ref[address](address.uninit)
      journal.journal_add1(ND1, ADR1, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        journal.index_store(KEY1, ADR1.get, ND1, ERR)
      } else {
        if (EXISTS.get != true) {
          journal.index_remove(KEY1)
        }
      }
    }
  }

  private def aubifs_iget_check(INO: Int, EXISTS: Ref[Boolean], INODE: Ref[inode], ERR: Ref[error]): Unit = {
    if (ERR.get == types.error.ESUCCESS) {
      val KEY: key = types.key.inodekey(INO)
      val ND = new Ref[node](node.uninit)
      journal.index_lookup(KEY, EXISTS, ND, ERR)
      if (ERR.get == types.error.ESUCCESS && EXISTS.get)
        INODE := types.inode.mkinode(INO, ND.get.meta, ND.get.directory, ND.get.nlink, ND.get.size)
      
    }
  }

  private def aubifs_pget_check(P_INO: Int, EXISTS: Ref[Boolean], P_INODE: Ref[inode], ERR: Ref[error]): Unit = {
    val INODE = new Ref[inode](inode.uninit)
    val INO: Int = P_INO
    aubifs_iget_check(INO, EXISTS, INODE, ERR)
    P_INODE := INODE.get
  }

  def aubifs_replay(ERR: Ref[error]): Unit = {
    val AX: address_list = new address_list()
    val KS: key_set = new key_set()
    journal.aubifs_readflash(AX, KS, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_replayorphans(KS, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_replaylog(AX, ERR)
    }
  }

  private def aubifs_replaylog(AX: address_list, ERR: Ref[error]): Unit = {
    val az: address_list = AX.deepCopy
    while (ERR.get == types.error.ESUCCESS && ! az.isEmpty) {
      val ADR: address = az.head
      aubifs_replayone(ADR, ERR)
      az.removeHead
    }
  }

  private def aubifs_replayone(ADR: address, ERR: Ref[error]): Unit = {
    val ND = new Ref[node](node.uninit)
    journal.journal_get(ADR, ND, ERR)
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    if (ERR.get == types.error.ESUCCESS) {
      journal.index_checkkey(ND.get.key, EXISTS, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      if (ND.get.isInstanceOf[types.node.inodenode]) {
        if (ND.get.nlink != 0 || ND.get.key.ino == ROOT_INO) {
          journal.index_store(ND.get.key, ADR, ND.get, ERR)
        } else {
          if (! ND.get.directory) {
            journal.index_checkdata(ND.get.key, 0, ERR)
            if (ERR.get == types.error.ESUCCESS) {
              journal.index_truncate(ND.get.key, 0, ERR)
            }
          }
          if (ERR.get == types.error.ESUCCESS) {
            journal.index_remove(ND.get.key)
          }
        }
      }
      if (ND.get.isInstanceOf[types.node.dentrynode]) {
        if (ND.get.ino != 0) {
          journal.index_store(ND.get.key, ADR, ND.get, ERR)
        } else {
          journal.index_remove(ND.get.key)
        }
      }
      if (ND.get.isInstanceOf[types.node.datanode]) {
        val KEY: key = types.key.inodekey(ND.get.key.ino)
        journal.index_contains(KEY, EXISTS, ERR)
        if (ERR.get == types.error.ESUCCESS && EXISTS.get) {
          journal.index_store(ND.get.key, ADR, ND.get, ERR)
        }
      }
      if (ND.get.isInstanceOf[types.node.truncnode]) {
        journal.index_checkdata(ND.get.key, ND.get.size, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          journal.index_truncate(ND.get.key, ND.get.size, ERR)
        }
      }
    }
  }

  private def aubifs_replayorphans(KS: key_set, ERR: Ref[error]): Unit = {
    while (! KS.isEmpty && ERR.get == types.error.ESUCCESS) {
      ChooseIn((KS).set, (KEY : key) =>
      {
        journal.index_truncate(KEY, 0, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          journal.index_remove(KEY)
        }
        if (ERR.get == types.error.ESUCCESS) {
          journal.orphan_remove(KEY)
          KS -= KEY
        }
      })
    }
  }

}

object AUBIFS {
  def apply(journal: AUBIFSJournal)(implicit _algebraic_implicit: algebraic.Algebraic) = {
    new AUBIFS(journal)
  }
}
