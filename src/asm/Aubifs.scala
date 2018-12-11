// Flashix: a verified file system for flash memory
// (c) 2015-2018 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._
import types._
import types.error.error

class Aubifs(var DoGc: Condition, var Lock: ReentrantReadWriteLock, val aubifs_core : AubifsCoreInterface)(implicit _algebraic_implicit: algebraic.Algebraic) extends AfsInterface {
  import _algebraic_implicit._

  override def check_commit(ERR: Ref[error]): Unit = {
    Lock.writeLock().lock()
    aubifs_core.check_commit(ERR)
    Lock.writeLock().unlock()
  }

  override def create(MD: metadata, P_INODE: inode, C_INODE: inode, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    val KEY3 = Ref[key](types.key.uninit)
    aubifs_core.index_newino(KEY3)
    val KEY1: key = types.key.inodekey(P_INODE.ino)
    val KEY2: key = types.key.dentrykey(P_INODE.ino, DENT.get.name)
    val ND1: node = types.node.inodenode(KEY1, P_INODE.meta, P_INODE.directory, P_INODE.nlink, P_INODE.nsubdirs, P_INODE.size + 1)
    val ND2: node = types.node.dentrynode(KEY2, KEY3.get.ino)
    val ND3: node = types.node.inodenode(KEY3.get, MD, false, 1, 0, 0)
    val ADR1 = Ref[address](types.address.uninit)
    val ADR2 = Ref[address](types.address.uninit)
    val ADR3 = Ref[address](types.address.uninit)

    Lock.writeLock().lock()
    aubifs_core.journal_add3(ND1, ND2, ND3, ADR1, ADR2, ADR3, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_core.index_store(KEY1, ADR1.get)
      aubifs_core.index_store(KEY2, ADR2.get)
      aubifs_core.index_store(KEY3.get, ADR3.get)
    }
    DoGc.signal()
    Lock.writeLock().unlock()

    if (ERR.get == types.error.ESUCCESS) {
      DENT := types.dentry.mkdentry(DENT.get.name, KEY3.get.ino)
      P_INODE := types.inode.mkinode(P_INODE.ino, ND1.meta, ND1.directory, ND1.nlink, ND1.nsubdirs, ND1.size)
      C_INODE := types.inode.mkinode(KEY3.get.ino, ND3.meta, ND3.directory, ND3.nlink, ND3.nsubdirs, ND3.size)
    }
  }

  override def evict(INODE: inode, ERR: Ref[error]): Unit = {
    val KEY: key = types.key.inodekey(INODE.ino)
    val EXISTS = Ref[Boolean](helpers.scala.Boolean.uninit)

    Lock.writeLock().lock()
    aubifs_core.orphans_contains(KEY, EXISTS)
    if (EXISTS.get) {
      aubifs_core.index_truncate(KEY, 0)
      aubifs_core.orphans_remove(KEY)
      aubifs_core.index_remove(KEY)
    }
    DoGc.signal()
    Lock.writeLock().unlock()

    ERR := types.error.ESUCCESS
  }

  override def format(VOLSIZE: Int, DOSYNC: Boolean, SIZE: Int, MD: metadata, ERR: Ref[error]): Unit = {
    if (Lock != null) {
      Lock.writeLock().lock() // make sure nobody else still has this lock
    }

    Lock = new ReentrantReadWriteLock()
    DoGc = Lock.writeLock().newCondition()

    Lock.writeLock().lock()
    aubifs_core.format(VOLSIZE, SIZE, DOSYNC, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val KEY: key = types.key.inodekey(ROOT_INO)
      val ADR = Ref[address](types.address.uninit)
      val ND: node = types.node.inodenode(KEY, MD, true, 0, 0, 0)
      aubifs_core.journal_add1(ND, ADR, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        aubifs_core.index_store(KEY, ADR.get)
      }
      if (ERR.get == types.error.ESUCCESS) {
        aubifs_core.journal_sync(ERR)
      }
    }
    Lock.writeLock().unlock()
  }

  override def fsync(INODE: inode, ISDATASYNC: Boolean, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
  }

  override def fsyncdir(INODE: inode, ISDATASYNC: Boolean, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
  }

  def gc(): Unit = {
    Lock.writeLock().lock()
    aubifs_core.journal_gc()
    Lock.writeLock().unlock()
  }

  def do_gc(is_easy: Boolean, ERR: Ref[error], free_lebs: Int): Unit = {
    println("flashix: attempting garbage collection with %d LEBs (%s)" format (free_lebs, if (is_easy) "easy" else "critical"))

    check_commit(ERR)

    if (ERR.get == error.ESUCCESS) {
      Lock.writeLock().lock()
      aubifs_core.journal_gc()
      Lock.writeLock().unlock()
    } else {
      println(s"flashix: garbage collection impossible")
    }
  }

  def gc_critical(tryCommit: Boolean, free: Int, log: Int, ERR: Ref[error]): Unit = {
    if (tryCommit && log != 0 && free == 0) {
      val total_bytes = Ref[Int](0)
      val free_bytes = Ref[Int](0)
      val LEB_SIZE = Ref[Int](0)

      aubifs_core.compute_stats(total_bytes, free_bytes, LEB_SIZE)

      if (free_bytes.get > LEB_SIZE.get * (4 + free)) {
        println(s"flashix: attempting to free $free_bytes bytes from the log")
        Lock.writeLock().lock()
        aubifs_core.commit(ERR)
        Lock.writeLock().unlock()

        gc_loop(tryCommit, log, ERR)
      }
    } else {
      println(s"flashix: no blocks available for garbage collection")
    }
  }

  // adapted from: integration/fuse/FilesystemAdapter GCloop
  def gc_loop(tryCommit: Boolean, log: Int, ERR: Ref[error]): Unit = {
    val (total, free) = (Ref[Int](0), Ref[Int](0))
    aubifs_core.main_area_LEBs(total, free)
    val isCritical = free.get < percent_of(10, total.get)

    val N = new Ref[Int](0)
    aubifs_core.get_gc_block(N, ERR)

    if (ERR.get == error.ESUCCESS) {
      val isEasy = Ref[Boolean](false)
      val eligible = Ref[Boolean](false)

      aubifs_core.is_block_eligible_for_gc(N.get, eligible)
      aubifs_core.is_gc_easy(N.get, isEasy)

      if (!eligible.get) {
        if (isCritical)
          gc_critical(tryCommit, free.get, log, ERR)
      } else if (isEasy.get || isCritical) {
        do_gc(isEasy.get, ERR, free.get)
        if (ERR.get == error.ESUCCESS)
          gc_loop(tryCommit, log, ERR)
      }
    } else {
      gc_critical(tryCommit, free.get, log, ERR)
    }
  }

  def gc_worker(tryCommit: Boolean, log: Int, ERR: Ref[error]): Unit = {
    Lock.writeLock().lock()
    DoGc.await()
    Lock.writeLock().unlock()

    gc_loop(tryCommit, log, ERR)
  }

  override def iget(INO: Int, INODE: inode, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val EXISTS = Ref[Boolean](helpers.scala.Boolean.uninit)
    iget_check(INO, EXISTS, INODE, ERR)
  }

  def iget_check(INO: Int, EXISTS: Ref[Boolean], INODE: inode, ERR: Ref[error]): Unit = {
    if (ERR.get == types.error.ESUCCESS) {
      val KEY: key = types.key.inodekey(INO)
      val ND = Ref[node](types.node.uninit)

      Lock.readLock().lock()
      aubifs_core.index_lookup(KEY, EXISTS, ND, ERR)
      Lock.readLock().unlock()

      if (ERR.get == types.error.ESUCCESS && EXISTS.get) {
        INODE := types.inode.mkinode(INO, ND.get.meta, ND.get.directory, ND.get.nlink, if (ND.get.directory) ND.get.nsubdirs else 0, ND.get.size)
      }
    }
  }

  override def link(OLD_DENT: dentry, P_INODE: inode, C_INODE: inode, NEW_DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    val KEY1: key = types.key.inodekey(P_INODE.ino)
    val KEY2: key = types.key.dentrykey(P_INODE.ino, NEW_DENT.get.name)
    val INO: Int = C_INODE.ino
    val KEY3: key = types.key.inodekey(INO)
    val ND1: node = types.node.inodenode(KEY1, P_INODE.meta, P_INODE.directory, P_INODE.nlink, P_INODE.nsubdirs, P_INODE.size + 1)
    val ND2: node = types.node.dentrynode(KEY2, INO)
    val ND3: node = types.node.inodenode(KEY3, C_INODE.meta, C_INODE.directory, C_INODE.nlink + 1, C_INODE.nsubdirs, C_INODE.size)
    val ADR1 = Ref[address](types.address.uninit)
    val ADR2 = Ref[address](types.address.uninit)
    val ADR3 = Ref[address](types.address.uninit)

    Lock.writeLock().lock()
    aubifs_core.journal_add3(ND1, ND2, ND3, ADR1, ADR2, ADR3, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_core.index_store(KEY1, ADR1.get)
      aubifs_core.index_store(KEY2, ADR2.get)
      aubifs_core.index_store(KEY3, ADR3.get)
    }
    DoGc.signal()
    Lock.writeLock().unlock()

    if (ERR.get == types.error.ESUCCESS) {
      NEW_DENT := types.dentry.mkdentry(NEW_DENT.get.name, INO)
      P_INODE := types.inode.mkinode(P_INODE.ino, ND1.meta, ND1.directory, ND1.nlink, ND1.nsubdirs, ND1.size)
      C_INODE := types.inode.mkinode(INO, ND3.meta, ND3.directory, ND3.nlink, ND3.nsubdirs, ND3.size)
    }
  }

  override def lookup(P_INO: Int, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val KEY: key = types.key.dentrykey(P_INO, DENT.get.name)
    val ND = Ref[node](types.node.uninit)
    val EXISTS = Ref[Boolean](helpers.scala.Boolean.uninit)

    Lock.readLock().lock()
    aubifs_core.index_lookup(KEY, EXISTS, ND, ERR)
    Lock.readLock().unlock()

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

  override def mkdir(MD: metadata, P_INODE: inode, C_INODE: inode, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    val KEY3 = Ref[key](types.key.uninit)

    Lock.writeLock().lock()
    aubifs_core.index_newino(KEY3)

    val KEY1: key = types.key.inodekey(P_INODE.ino)
    val KEY2: key = types.key.dentrykey(P_INODE.ino, DENT.get.name)
    val ND1: node = types.node.inodenode(KEY1, P_INODE.meta, P_INODE.directory, P_INODE.nlink, P_INODE.nsubdirs + 1, P_INODE.size + 1)
    val ND2: node = types.node.dentrynode(KEY2, KEY3.get.ino)
    val ND3: node = types.node.inodenode(KEY3.get, MD, true, 1, 0, 0)
    val ADR1 = Ref[address](types.address.uninit)
    val ADR2 = Ref[address](types.address.uninit)
    val ADR3 = Ref[address](types.address.uninit)

    aubifs_core.journal_add3(ND1, ND2, ND3, ADR1, ADR2, ADR3, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_core.index_store(KEY1, ADR1.get)
      aubifs_core.index_store(KEY2, ADR2.get)
      aubifs_core.index_store(KEY3.get, ADR3.get)
    }
    DoGc.signal()
    Lock.writeLock().unlock()

    if (ERR.get == types.error.ESUCCESS) {
      DENT := types.dentry.mkdentry(DENT.get.name, KEY3.get.ino)
      P_INODE := types.inode.mkinode(P_INODE.ino, ND1.meta, ND1.directory, ND1.nlink, ND1.nsubdirs, ND1.size)
      C_INODE := types.inode.mkinode(KEY3.get.ino, ND3.meta, ND3.directory, ND3.nlink, ND3.nsubdirs, ND3.size)
    }
  }

  override def readdir(INODE: inode, NAMES: stringset, ERR: Ref[error]): Unit = {
    val KEY: key = types.key.inodekey(INODE.ino)

    Lock.readLock().lock()
    aubifs_core.index_entries(KEY, NAMES, ERR)
    Lock.readLock().unlock()
  }

  override def readpage(INODE: inode, PAGENO: Int, PBUF: buffer, EXISTS: Ref[Boolean], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val ND = Ref[node](types.node.uninit)
    val KEY: key = types.key.datakey(INODE.ino, PAGENO)

    Lock.readLock().lock()
    aubifs_core.index_lookup(KEY, EXISTS, ND, ERR)
    Lock.readLock().unlock()

    if (ERR.get == types.error.ESUCCESS) {
      if (EXISTS.get) {
        PBUF.copy(ND.get.data, 0, 0, VFS_PAGE_SIZE)
      } else {
        PBUF.fill(zero)
      }
    }
  }

  override def recovery(DOSYNC: Boolean, ERR: Ref[error]): Unit = {
    val AX: address_list = new address_list()
    val KS: key_set = new key_set()

    if (Lock != null) {
      Lock.writeLock().lock() // make sure nobody else still has this lock
    }
    Lock = new ReentrantReadWriteLock()
    DoGc = Lock.writeLock().newCondition()

    Lock.writeLock().lock()
    aubifs_core.recover(DOSYNC, AX, KS, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      replayorphans(KS)
    }
    if (ERR.get == types.error.ESUCCESS) {
      replaylog(AX, ERR)
    }
    Lock.writeLock().unlock()
  }

  override def rename(OLD_CHILD_INODE: inode, OLD_PARENT_INODE: inode, NEW_PARENT_INODE: inode, NEW_CHILD_INODE: inode, OLD_DENT: Ref[dentry], NEW_DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    val OVERWRITE: Boolean = NEW_DENT.get.isInstanceOf[types.dentry.mkdentry]
    val REPARENT: Boolean = OLD_PARENT_INODE.ino != NEW_PARENT_INODE.ino
    val IS_DIR: Boolean = OLD_CHILD_INODE.directory
    if (REPARENT) {
      if (OVERWRITE) {
        rename_overwrite_reparent(IS_DIR, OLD_CHILD_INODE, OLD_PARENT_INODE, NEW_PARENT_INODE, NEW_CHILD_INODE, OLD_DENT, NEW_DENT, ERR)
      } else {
        rename_new_reparent(IS_DIR, OLD_CHILD_INODE, OLD_PARENT_INODE, NEW_PARENT_INODE, OLD_DENT, NEW_DENT, ERR)
      }
    } else {
      if (OVERWRITE) {
        rename_overwrite_keep_parent(IS_DIR, OLD_CHILD_INODE, OLD_PARENT_INODE, NEW_CHILD_INODE, OLD_DENT, NEW_DENT, ERR)
      } else {
        rename_new_keep_parent(IS_DIR, OLD_CHILD_INODE, OLD_PARENT_INODE, OLD_DENT, NEW_DENT, ERR)
      }
      NEW_PARENT_INODE := OLD_PARENT_INODE.deepCopy
    }
  }

  def rename_new_keep_parent(IS_DIR: Boolean, OLD_CHILD_INODE: inode, PARENT_INODE: inode, OLD_DENT: Ref[dentry], NEW_DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    val KEY1: key = types.key.dentrykey(PARENT_INODE.ino, OLD_DENT.get.name)
    val ND1: node = types.node.dentrynode(KEY1, 0)
    val KEY2: key = types.key.dentrykey(PARENT_INODE.ino, NEW_DENT.get.name)
    val ND2: node = types.node.dentrynode(KEY2, OLD_DENT.get.ino)
    val KEY3: key = types.key.inodekey(PARENT_INODE.ino)
    val ND3: node = types.node.inodenode(KEY3, PARENT_INODE.meta, PARENT_INODE.directory, PARENT_INODE.nlink, PARENT_INODE.nsubdirs, PARENT_INODE.size)
    val ADR2 = Ref[address](types.address.uninit)
    val ADR3 = Ref[address](types.address.uninit)
    val ADR1 = Ref[address](types.address.uninit)

    Lock.writeLock().lock()
    aubifs_core.journal_add3(ND1, ND2, ND3, ADR1, ADR2, ADR3, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_core.index_remove(KEY1)
      aubifs_core.index_store(KEY2, ADR2.get)
      aubifs_core.index_store(KEY3, ADR3.get)
    }
    DoGc.signal()
    Lock.writeLock().unlock()

    if (ERR.get == types.error.ESUCCESS) {
      NEW_DENT := types.dentry.mkdentry(NEW_DENT.get.name, OLD_DENT.get.ino)
      OLD_DENT := types.dentry.negdentry(OLD_DENT.get.name)
      PARENT_INODE := types.inode.mkinode(PARENT_INODE.ino, ND3.meta, ND3.directory, ND3.nlink, ND3.nsubdirs, ND3.size)
    }
  }

  def rename_new_reparent(IS_DIR: Boolean, OLD_CHILD_INODE: inode, OLD_PARENT_INODE: inode, NEW_PARENT_INODE: inode, OLD_DENT: Ref[dentry], NEW_DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    val KEY1: key = types.key.dentrykey(OLD_PARENT_INODE.ino, OLD_DENT.get.name)
    val ND1: node = types.node.dentrynode(KEY1, 0)
    val KEY2: key = types.key.dentrykey(NEW_PARENT_INODE.ino, NEW_DENT.get.name)
    val ND2: node = types.node.dentrynode(KEY2, OLD_DENT.get.ino)
    val KEY3: key = types.key.inodekey(OLD_PARENT_INODE.ino)
    val ND3: node = types.node.inodenode(KEY3, OLD_PARENT_INODE.meta, OLD_PARENT_INODE.directory, OLD_PARENT_INODE.nlink, OLD_PARENT_INODE.nsubdirs - (if (IS_DIR) 1 else 0), OLD_PARENT_INODE.size - 1)
    val KEY4: key = types.key.inodekey(NEW_PARENT_INODE.ino)
    val ND4: node = types.node.inodenode(KEY4, NEW_PARENT_INODE.meta, NEW_PARENT_INODE.directory, NEW_PARENT_INODE.nlink, NEW_PARENT_INODE.nsubdirs + (if (IS_DIR) 1 else 0), NEW_PARENT_INODE.size + 1)
    val ADR4 = Ref[address](types.address.uninit)
    val ADR2 = Ref[address](types.address.uninit)
    val ADR3 = Ref[address](types.address.uninit)
    val ADR1 = Ref[address](types.address.uninit)

    Lock.writeLock().lock()
    aubifs_core.journal_add4(ND1, ND2, ND3, ND4, ADR1, ADR2, ADR3, ADR4, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_core.index_remove(KEY1)
      aubifs_core.index_store(KEY2, ADR2.get)
      aubifs_core.index_store(KEY3, ADR3.get)
      aubifs_core.index_store(KEY4, ADR4.get)
    }
    DoGc.signal()
    Lock.writeLock().unlock()

    if (ERR.get == types.error.ESUCCESS) {
      NEW_DENT := types.dentry.mkdentry(NEW_DENT.get.name, OLD_DENT.get.ino)
      OLD_DENT := types.dentry.negdentry(OLD_DENT.get.name)
      OLD_PARENT_INODE := types.inode.mkinode(OLD_PARENT_INODE.ino, ND3.meta, ND3.directory, ND3.nlink, ND3.nsubdirs, ND3.size)
      NEW_PARENT_INODE := types.inode.mkinode(NEW_PARENT_INODE.ino, ND4.meta, ND4.directory, ND4.nlink, ND4.nsubdirs, ND4.size)
    }
  }

  def rename_overwrite_keep_parent(IS_DIR: Boolean, OLD_CHILD_INODE: inode, PARENT_INODE: inode, NEW_CHILD_INODE: inode, OLD_DENT: Ref[dentry], NEW_DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    var KEY1: key = types.key.uninit
    val IDENTITY: Boolean = OLD_DENT.get == NEW_DENT.get
    KEY1 = types.key.dentrykey(PARENT_INODE.ino, OLD_DENT.get.name)
    val ND1: node = types.node.dentrynode(KEY1, 0)
    val KEY2: key = types.key.dentrykey(PARENT_INODE.ino, NEW_DENT.get.name)
    val KEY3: key = types.key.inodekey(PARENT_INODE.ino)
    val KEY4: key = types.key.inodekey(NEW_DENT.get.ino)
    var ND3: node = types.node.uninit
    var ND4: node = types.node.uninit
    if (IDENTITY != true) {
      ND3 = types.node.inodenode(KEY3, PARENT_INODE.meta, PARENT_INODE.directory, PARENT_INODE.nlink, PARENT_INODE.nsubdirs - (if (IS_DIR) 1 else 0), PARENT_INODE.size - 1)
      ND4 = types.node.inodenode(KEY4, NEW_CHILD_INODE.meta, NEW_CHILD_INODE.directory, NEW_CHILD_INODE.nlink - 1, NEW_CHILD_INODE.nsubdirs, NEW_CHILD_INODE.size)
    } else {
      ND3 = types.node.inodenode(KEY3, PARENT_INODE.meta, PARENT_INODE.directory, PARENT_INODE.nlink, PARENT_INODE.nsubdirs, PARENT_INODE.size)
      ND4 = types.node.inodenode(KEY4, NEW_CHILD_INODE.meta, NEW_CHILD_INODE.directory, NEW_CHILD_INODE.nlink, NEW_CHILD_INODE.nsubdirs, NEW_CHILD_INODE.size)
    }
    val ND2: node = types.node.dentrynode(KEY2, OLD_DENT.get.ino)
    val ADR4 = Ref[address](types.address.uninit)
    val ADR2 = Ref[address](types.address.uninit)
    val ADR3 = Ref[address](types.address.uninit)
    val ADR1 = Ref[address](types.address.uninit)

    Lock.writeLock().lock()
    aubifs_core.journal_add4(ND1, ND2, ND3, ND4, ADR1, ADR2, ADR3, ADR4, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_core.index_remove(KEY1)
      aubifs_core.index_store(KEY2, ADR2.get)
      aubifs_core.index_store(KEY3, ADR3.get)
      aubifs_core.index_store(KEY4, ADR4.get)
      if (ND4.nlink == 0) {
        aubifs_core.orphans_insert(KEY4)
      }
    }
    DoGc.signal()
    Lock.writeLock().unlock()

    if (ERR.get == types.error.ESUCCESS) {
      NEW_DENT := types.dentry.mkdentry(NEW_DENT.get.name, OLD_DENT.get.ino)
      OLD_DENT := types.dentry.negdentry(OLD_DENT.get.name)
      PARENT_INODE := types.inode.mkinode(PARENT_INODE.ino, ND3.meta, ND3.directory, ND3.nlink, ND3.nsubdirs, ND3.size)
      NEW_CHILD_INODE := types.inode.mkinode(NEW_CHILD_INODE.ino, ND4.meta, ND4.directory, ND4.nlink, ND4.nsubdirs, ND4.size)
    }
  }

  def rename_overwrite_reparent(IS_DIR: Boolean, OLD_CHILD_INODE: inode, OLD_PARENT_INODE: inode, NEW_PARENT_INODE: inode, NEW_CHILD_INODE: inode, OLD_DENT: Ref[dentry], NEW_DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    val KEY1: key = types.key.dentrykey(OLD_PARENT_INODE.ino, OLD_DENT.get.name)
    val ND1: node = types.node.dentrynode(KEY1, 0)
    val KEY2: key = types.key.dentrykey(NEW_PARENT_INODE.ino, NEW_DENT.get.name)
    val ND2: node = types.node.dentrynode(KEY2, OLD_DENT.get.ino)
    val KEY3: key = types.key.inodekey(OLD_PARENT_INODE.ino)
    val ND3: node = types.node.inodenode(KEY3, OLD_PARENT_INODE.meta, OLD_PARENT_INODE.directory, OLD_PARENT_INODE.nlink, OLD_PARENT_INODE.nsubdirs - (if (IS_DIR) 1 else 0), OLD_PARENT_INODE.size - 1)
    val KEY4: key = types.key.inodekey(NEW_PARENT_INODE.ino)
    val ND4: node = types.node.inodenode(KEY4, NEW_PARENT_INODE.meta, NEW_PARENT_INODE.directory, NEW_PARENT_INODE.nlink, NEW_PARENT_INODE.nsubdirs, NEW_PARENT_INODE.size)
    val KEY5: key = types.key.inodekey(NEW_DENT.get.ino)
    val ND5: node = types.node.inodenode(KEY5, NEW_CHILD_INODE.meta, NEW_CHILD_INODE.directory, NEW_CHILD_INODE.nlink - 1, NEW_CHILD_INODE.nsubdirs, NEW_CHILD_INODE.size)
    val ADR5 = Ref[address](types.address.uninit)
    val ADR4 = Ref[address](types.address.uninit)
    val ADR2 = Ref[address](types.address.uninit)
    val ADR3 = Ref[address](types.address.uninit)
    val ADR1 = Ref[address](types.address.uninit)

    Lock.writeLock().lock()
    aubifs_core.journal_add5(ND1, ND2, ND3, ND4, ND5, ADR1, ADR2, ADR3, ADR4, ADR5, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_core.index_remove(KEY1)
      aubifs_core.index_store(KEY2, ADR2.get)
      aubifs_core.index_store(KEY3, ADR3.get)
      aubifs_core.index_store(KEY4, ADR4.get)
      aubifs_core.index_store(KEY5, ADR5.get)
      if (ND5.nlink == 0) {
        aubifs_core.orphans_insert(KEY5)
      }
    }
    DoGc.signal()
    Lock.writeLock().unlock()

    if (ERR.get == types.error.ESUCCESS) {
      NEW_DENT := types.dentry.mkdentry(NEW_DENT.get.name, OLD_DENT.get.ino)
      OLD_DENT := types.dentry.negdentry(OLD_DENT.get.name)
      OLD_PARENT_INODE := types.inode.mkinode(OLD_PARENT_INODE.ino, ND3.meta, ND3.directory, ND3.nlink, ND3.nsubdirs, ND3.size)
      NEW_CHILD_INODE := types.inode.mkinode(NEW_CHILD_INODE.ino, ND5.meta, ND5.directory, ND5.nlink, ND5.nsubdirs, ND5.size)
    }
  }

  def replaylog(AX1: address_list, ERR: Ref[error]): Unit = {
    val AX: address_list = AX1.deepCopy
    while (ERR.get == types.error.ESUCCESS && ! AX.isEmpty) {
      val ADR: address = AX.head
      replayone(ADR, ERR)
      AX.removeHead
    }
  }

  def replayone(ADR: address, ERR: Ref[error]): Unit = {
    val ND = Ref[node](types.node.uninit)
    aubifs_core.journal_get(ADR, ND, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (ND.get.isInstanceOf[types.node.inodenode]) {
        if (ND.get.nlink != 0 || ND.get.key.ino == ROOT_INO) {
          aubifs_core.index_store(ND.get.key, ADR)
        } else {
          if (! ND.get.directory) {
            aubifs_core.index_truncate(ND.get.key, 0)
          }
          if (ERR.get == types.error.ESUCCESS) {
            aubifs_core.index_remove(ND.get.key)
          }
        }
      }
      if (ND.get.isInstanceOf[types.node.dentrynode]) {
        if (ND.get.ino != 0) {
          aubifs_core.index_store(ND.get.key, ADR)
        } else {
          aubifs_core.index_remove(ND.get.key)
        }
      }
      if (ND.get.isInstanceOf[types.node.datanode]) {
        val EXISTS = Ref[Boolean](helpers.scala.Boolean.uninit)
        val KEY: key = types.key.inodekey(ND.get.key.ino)
        aubifs_core.index_contains(KEY, EXISTS, ERR)
        if (ERR.get == types.error.ESUCCESS && EXISTS.get) {
          aubifs_core.index_store(ND.get.key, ADR)
        }
      }
      if (ND.get.isInstanceOf[types.node.truncnode]) {
        aubifs_core.index_truncate(ND.get.key, ND.get.size)
      }
    }
  }

  def replayorphans(KS: key_set): Unit = {
    while (! KS.isEmpty) {
      val KEY: key = KS.head
      aubifs_core.index_truncate(KEY, 0)
      aubifs_core.index_remove(KEY)
      aubifs_core.orphans_remove(KEY)
      KS -= KEY
    }
  }

  override def rmdir(P_INODE: inode, C_INODE: inode, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    val KEY1: key = types.key.inodekey(P_INODE.ino)
    val KEY2: key = types.key.dentrykey(P_INODE.ino, DENT.get.name)
    val INO: Int = C_INODE.ino
    val KEY3: key = types.key.inodekey(C_INODE.ino)
    val ND1: node = types.node.inodenode(KEY1, P_INODE.meta, P_INODE.directory, P_INODE.nlink, P_INODE.nsubdirs - 1, P_INODE.size - 1)
    val ND2: node = types.node.dentrynode(KEY2, 0)
    val ND3: node = types.node.inodenode(KEY3, C_INODE.meta, C_INODE.directory, C_INODE.nlink - 1, C_INODE.nsubdirs, C_INODE.size)
    val ADR1 = Ref[address](types.address.uninit)
    val ADR3 = Ref[address](types.address.uninit)
    val ADR2 = Ref[address](types.address.uninit)

    Lock.writeLock().lock()
    aubifs_core.journal_add3(ND1, ND2, ND3, ADR1, ADR2, ADR3, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_core.index_store(KEY1, ADR1.get)
      aubifs_core.index_remove(KEY2)
      aubifs_core.index_store(KEY3, ADR3.get)
      aubifs_core.orphans_insert(KEY3)
    }
    DoGc.signal()
    Lock.writeLock().unlock()

    if (ERR.get == types.error.ESUCCESS) {
      DENT := types.dentry.negdentry(DENT.get.name)
      P_INODE := types.inode.mkinode(P_INODE.ino, ND1.meta, ND1.directory, ND1.nlink, ND1.nsubdirs, ND1.size)
      C_INODE := types.inode.mkinode(INO, ND3.meta, ND3.directory, ND3.nlink, ND3.nsubdirs, ND3.size)
    }
  }

  override def sync(ERR: Ref[error]): Unit = {
    Lock.writeLock().lock()
    aubifs_core.journal_sync(ERR)
    Lock.writeLock().unlock()
  }

  override def truncate(N: Int, PAGENO: Int, PBUF_OPT: Ref[buffer_opt], INODE: inode, ERR: Ref[error]): Unit = {
    val EXISTS = Ref[Boolean](helpers.scala.Boolean.uninit)
    val ADR3 = Ref[address](types.address.uninit)
    val ADR2 = Ref[address](types.address.uninit)
    val ADR1 = Ref[address](types.address.uninit)
    val KEY1: key = types.key.inodekey(INODE.ino)
    val KEY2: key = types.key.inodekey(INODE.ino)
    val OFFSET: Int = INODE.size % VFS_PAGE_SIZE
    val SIZE: Int = min(N, INODE.size)
    val KEY3: key = types.key.datakey(INODE.ino, PAGENO)
    val ND2: node = types.node.inodenode(KEY2, INODE.meta, INODE.directory, INODE.nlink, INODE.nsubdirs, N)
    val ND3 = Ref[node](types.node.uninit)

    Lock.writeLock().lock()
    if (INODE.size <= N && OFFSET != 0) {
      if (PBUF_OPT.get.isInstanceOf[types.buffer_opt.some]) {
        ND3 := types.node.datanode(KEY3, PBUF_OPT.get.buf).deepCopy
        EXISTS := true
        ERR := types.error.ESUCCESS
      } else {
        aubifs_core.index_lookup(KEY3, EXISTS, ND3, ERR)
      }
    } else {
      EXISTS := false
      ERR := types.error.ESUCCESS
    }
    val ND1: node = types.node.truncnode(KEY1, SIZE)
    if (ERR.get == types.error.ESUCCESS) {
      if (EXISTS.get) {
        ND3.get.data.fill(zero, OFFSET, VFS_PAGE_SIZE - OFFSET)
        aubifs_core.journal_add3(ND1, ND2, ND3.get, ADR1, ADR2, ADR3, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          aubifs_core.index_store(KEY3, ADR3.get)
        }
      } else {
        aubifs_core.journal_add2(ND1, ND2, ADR1, ADR2, ERR)
      }
    }
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_core.index_store(KEY2, ADR2.get)
      aubifs_core.index_truncate(KEY1, SIZE)
    }
    DoGc.signal()
    Lock.writeLock().unlock()

    if (ERR.get == types.error.ESUCCESS) {
      INODE := types.inode.mkinode(INODE.ino, ND2.meta, ND2.directory, ND2.nlink, ND2.nsubdirs, ND2.size)
      if (EXISTS.get) {
        PBUF_OPT := types.buffer_opt.some(ND3.get.data).deepCopy
      }
    }
  }

  override def unlink(P_INODE: inode, C_INODE: inode, DENT: Ref[dentry], ERR: Ref[error]): Unit = {
    val KEY1: key = types.key.inodekey(P_INODE.ino)
    val KEY2: key = types.key.dentrykey(P_INODE.ino, DENT.get.name)
    val INO: Int = C_INODE.ino
    val KEY3: key = types.key.inodekey(C_INODE.ino)
    val ND1: node = types.node.inodenode(KEY1, P_INODE.meta, P_INODE.directory, P_INODE.nlink, P_INODE.nsubdirs, P_INODE.size - 1)
    val ND2: node = types.node.dentrynode(KEY2, 0)
    val ND3: node = types.node.inodenode(KEY3, C_INODE.meta, C_INODE.directory, C_INODE.nlink - 1, C_INODE.nsubdirs, C_INODE.size)
    val ADR1 = Ref[address](types.address.uninit)
    val ADR3 = Ref[address](types.address.uninit)
    val ADR2 = Ref[address](types.address.uninit)

    Lock.writeLock().lock()
    aubifs_core.journal_add3(ND1, ND2, ND3, ADR1, ADR2, ADR3, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_core.index_store(KEY1, ADR1.get)
      aubifs_core.index_remove(KEY2)
      aubifs_core.index_store(KEY3, ADR3.get)
      if (C_INODE.nlink == 1) {
        aubifs_core.orphans_insert(KEY3)
      }
    }
    DoGc.signal()
    Lock.writeLock().unlock()

    if (ERR.get == types.error.ESUCCESS) {
      DENT := types.dentry.negdentry(DENT.get.name)
      P_INODE := types.inode.mkinode(P_INODE.ino, ND1.meta, ND1.directory, ND1.nlink, ND1.nsubdirs, ND1.size)
      C_INODE := types.inode.mkinode(INO, ND3.meta, ND3.directory, ND3.nlink, ND3.nsubdirs, ND3.size)
    }
  }

  override def write_meta(INODE: inode, MD: metadata, ERR: Ref[error]): Unit = {
    val KEY: key = types.key.inodekey(INODE.ino)
    val ADR = Ref[address](types.address.uninit)
    val ND: node = types.node.inodenode(KEY, MD, INODE.directory, INODE.nlink, INODE.nsubdirs, INODE.size)

    Lock.writeLock().lock()
    aubifs_core.journal_add1(ND, ADR, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_core.index_store(KEY, ADR.get)
    }
    DoGc.signal()
    Lock.writeLock().unlock()
  }

  override def write_size(INODE: inode, SIZE: Int, ERR: Ref[error]): Unit = {
    val KEY: key = types.key.inodekey(INODE.ino)
    val ADR = Ref[address](types.address.uninit)
    val ND: node = types.node.inodenode(KEY, INODE.meta, INODE.directory, INODE.nlink, INODE.nsubdirs, SIZE)

    Lock.writeLock().lock()
    aubifs_core.journal_add1(ND, ADR, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_core.index_store(KEY, ADR.get)
    }
    DoGc.signal()
    Lock.writeLock().unlock()
  }

  override def writepage(INODE: inode, PAGENO: Int, PBUF: buffer, ERR: Ref[error]): Unit = {
    var KEY1: key = types.key.uninit
    val INO: Int = INODE.ino
    KEY1 = types.key.datakey(INO, PAGENO)
    val ND1: node = types.node.datanode(KEY1, PBUF).deepCopy
    val ADR1 = Ref[address](types.address.uninit)

    Lock.writeLock().lock()
    aubifs_core.journal_add1(ND1, ADR1, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      aubifs_core.index_store(KEY1, ADR1.get)
    } else {
      val EXISTS: Boolean = true
      if (EXISTS != true) {
        aubifs_core.index_remove(KEY1)
      }
    }
    DoGc.signal()
    Lock.writeLock().unlock()
  }

}
