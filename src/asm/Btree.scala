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

class Btree(var ADRT : address, var RT : znode, val apersistence : ApersistenceInterface)(implicit _algebraic_implicit: algebraic.Algebraic, _procedures_implicit: proc.Procedures) extends IndexInterface {
  import _algebraic_implicit._
  import _procedures_implicit._

  override def add_gnds(GNDLIST: group_node_list, ADRLIST: address_list, ERR: Ref[error]): Unit = {
    apersistence.add_gnds(GNDLIST, ADRLIST, ERR)
  }

  override def allocate_gnd(ERR: Ref[error]): Unit = {
    apersistence.allocate_gnd(ERR)
  }

  override def commit(MAXINO0: Int, OS0: nat_set, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val ADR = Ref[address](ADRT)
    var DONE: Boolean = false
    if (RT != null && RT.dirty) {
      apersistence.allocate_ind(ERR)
      if (ERR.get == types.error.ESUCCESS) {
        commit_rec(RT, ADR, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        DONE = true
        apersistence.sync(ERR)
      }
    }
    if (ERR.get == types.error.ESUCCESS) {
      apersistence.commit(ADR.get, MAXINO0, OS0, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      ADRT = ADR.get
    } else     if (DONE) {
      RT.dirty = true
      val SIZE = Ref[Int](0)
      apersistence.get_iblock_refsize(ADR.get.lnum, SIZE)
      apersistence.set_iblock_refsize(ADR.get.lnum, SIZE.get - ADR.get.size)
    }
  }

  def commit_rec(R: znode, ADR: Ref[address], ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    if (! R.leaf) {
      var N: Int = 0
      while (N < R.usedsize && ERR.get == types.error.ESUCCESS) {
        val ADRC = Ref[address](R.zbranches(N).adr)
        val RC: znode = R.zbranches(N).child
        if (RC != null && RC.dirty) {
          commit_rec(RC, ADRC, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            R.zbranches(N) = R.zbranches(N).updated_adr(ADRC.get)
          }
        }
        N = N + 1
      }
    }
    if (ERR.get == types.error.ESUCCESS) {
      io_save(R, ADR, ERR)
    }
  }

  override def deallocate_gnd(N: Int, ERR: Ref[error]): Unit = {
    apersistence.deallocate_gnd(N, ERR)
  }

  def entries(KEY: key, RP: znode, ADR0: address, R: Ref[znode], DONE: Ref[Boolean], NAMES: stringset, ERR: Ref[error]): Unit = {
    io_load(RP, ADR0, R, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (R.get.leaf) {
        val N = Ref[Int](0)
        io_scan(KEY, 0, R.get, N)
        while (N.get < R.get.usedsize && (KEY.ino == R.get.zbranches(N.get).key.ino && R.get.zbranches(N.get).key.isInstanceOf[types.key.dentrykey])) {
          NAMES += R.get.zbranches(N.get).key.name
          N := N.get + 1
        }
        DONE := (N.get < R.get.usedsize)
      } else {
        val N = Ref[Int](0)
        io_scan(KEY, 1, R.get, N)
        while (N.get < R.get.usedsize && DONE.get != true) {
          val RC = Ref[znode](R.get.zbranches(N.get).child)
          val ADRC: address = R.get.zbranches(N.get).adr
          entries(KEY, R.get, ADRC, RC, DONE, NAMES, ERR)
          N := N.get + 1
        }
      }
    }
  }

  override def format(VOLSIZE: Int, SIZE: Int, MAXINO0: Int, ERR: Ref[error]): Unit = {
    RT = null
    apersistence.format(VOLSIZE, SIZE, MAXINO0, ERR)
    val N = Ref[Int](0)
    apersistence.get_leb_size(N)
    if (ERR.get == types.error.ESUCCESS) {
      val IND: index_node = types.index_node.indexnode(new branch_array(BRANCH_SIZE).fill(types.branch.uninit), true, 0)
      val SIZE: Int = flashsize(IND)
      if (SIZE > N.get) {
        debug("btree: initial index node to large")
        ERR := types.error.ENOSPC
      }
      if (ERR.get == types.error.ESUCCESS) {
        apersistence.allocate_ind(ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        
        {
          val address_variable0: Ref[address] = Ref[address](ADRT)
          apersistence.add_ind(IND, address_variable0, ERR)
          ADRT = address_variable0.get
        }
      }
      if (ERR.get == types.error.ESUCCESS) {
        apersistence.set_iblock_refsize(ADRT.lnum, SIZE)
      }
      if (ERR.get == types.error.ESUCCESS) {
        apersistence.sync(ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        val NS: nat_set = new nat_set()
        apersistence.commit(ADRT, MAXINO0, NS, ERR)
      }
    }
  }

  override def get_block_free_size(N: Ref[Int]): Unit = {
    apersistence.get_block_free_size(N)
  }

  override def get_gblock_refsize(LNUM: Int, N: Ref[Int]): Unit = {
    apersistence.get_gblock_refsize(LNUM, N)
  }

  override def get_gc_block(N: Ref[Int], ERR: Ref[error]): Unit = {
    apersistence.get_gc_block(N, ERR)
  }

  override def get_leb_size(N: Ref[Int]): Unit = {
    apersistence.get_leb_size(N)
  }

  override def index_contains(KEY: key, EXISTS: Ref[Boolean], ERR: Ref[error]): Unit = {
    val ADR = Ref[address](types.address.uninit)
    val DIRTY = Ref[Boolean](helpers.scala.Boolean.uninit)
    traverse_root(types.modification.contains(KEY), null, DIRTY, EXISTS, ADR, ERR)
  }

  override def index_entries(KEY: key, NAMES: stringset, ERR: Ref[error]): Unit = {
    val DONE = Ref[Boolean](false)
    
    {
      val ref_variable0: Ref[znode] = Ref[znode](RT)
      entries(types.key.dentrykey(KEY.ino, ""), null, ADRT, ref_variable0, DONE, NAMES, ERR)
      RT = ref_variable0.get
    }
  }

  override def index_lookup(KEY: key, EXISTS: Ref[Boolean], ADR: Ref[address], ERR: Ref[error]): Unit = {
    val DIRTY = Ref[Boolean](helpers.scala.Boolean.uninit)
    traverse_root(types.modification.lookup(KEY), null, DIRTY, EXISTS, ADR, ERR)
  }

  override def index_remove(KEY: key, OLDADR: Ref[address], EXISTS: Ref[Boolean]): Unit = {
    val ERR = Ref[error](types.error.uninit)
    val DIRTY = Ref[Boolean](helpers.scala.Boolean.uninit)
    traverse_root(types.modification.remove(KEY), null, DIRTY, EXISTS, OLDADR, ERR)
  }

  override def index_store(KEY: key, ADR: address, OLDADR: Ref[address], EXISTS: Ref[Boolean]): Unit = {
    val ERR = Ref[error](types.error.uninit)
    val DIRTY = Ref[Boolean](helpers.scala.Boolean.uninit)
    traverse_root(types.modification.store(KEY, ADR), null, DIRTY, EXISTS, OLDADR, ERR)
  }

  override def index_truncate(KEY: key, N: Int, AS: address_set): Unit = {
    val DIRTY = Ref[Boolean](false)
    val DONE = Ref[Boolean](false)
    val ERR = Ref[error](types.error.uninit)
    
    {
      val ref_variable0: Ref[znode] = Ref[znode](RT)
      truncate(types.key.datakey(KEY.ino, ((N + VFS_PAGE_SIZE) - 1) / VFS_PAGE_SIZE), null, ADRT, ref_variable0, DIRTY, DONE, AS, ERR)
      RT = ref_variable0.get
    }
  }

  def io_dirty(R: znode, ADR: address, DIRTY: Boolean, ERR: Ref[error]): Unit = {
    if (DIRTY) {
      if (! R.dirty) {
        val SIZE = Ref[Int](0)
        apersistence.get_iblock_refsize(ADR.lnum, SIZE)
        apersistence.set_iblock_refsize(ADR.lnum, SIZE.get - ADR.size)
      }
      R.dirty = true
    }
  }

  def io_load(R: znode, ADR: address, RN: Ref[znode], ERR: Ref[error]): Unit = {
    if (RN.get == null) {
      val IND = Ref[index_node](types.index_node.uninit)
      apersistence.read_ind(ADR, IND, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val R0 = Ref(types.znode.uninit)
        R0.get = types.znode.mkznode(R, new zbranch_array(BRANCH_SIZE).fill(types.zbranch.uninit), IND.get.leaf, false, IND.get.usedsize)
        RN := R0.get
        var N: Int = 0
        while (N < IND.get.usedsize) {
          RN.get.zbranches(N) = load(IND.get.branches(N))
          N = N + 1
        }
      }
    } else {
      ERR := types.error.ESUCCESS
    }
  }

  def io_save(R: znode, ADR: Ref[address], ERR: Ref[error]): Unit = {
    val IND: index_node = save(R).deepCopy
    val SIZE = Ref[Int](0)
    apersistence.get_block_free_size(SIZE)
    if (SIZE.get < flashsize(IND)) {
      apersistence.sync(ERR)
      if (ERR.get == types.error.ESUCCESS) {
        apersistence.allocate_ind(ERR)
      }
    } else {
      ERR := types.error.ESUCCESS
    }
    if (ERR.get == types.error.ESUCCESS) {
      apersistence.add_ind(IND, ADR, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val SIZE = Ref[Int](0)
        apersistence.get_iblock_refsize(ADR.get.lnum, SIZE)
        apersistence.set_iblock_refsize(ADR.get.lnum, SIZE.get + ADR.get.size)
      }
    }
    if (ERR.get == types.error.ESUCCESS) {
      R.dirty = false
    }
  }

  def io_scan(KEY: key, SKIP: Int, R: znode, POSITION: Ref[Int]): Unit = {
    POSITION := 0
    while (POSITION.get + SKIP < R.usedsize && <(R.zbranches(POSITION.get).key, KEY)) {
      POSITION := POSITION.get + 1
    }
  }

  def io_shift_left(POSITION: Int, R: znode): Unit = {
    var N: Int = POSITION
    while (N + 1 < R.usedsize) {
      R.zbranches(N) = R.zbranches(N + 1)
      N = N + 1
    }
  }

  def io_shift_n_left(POSITION: Int, N: Int, R: znode): Unit = {
    var M: Int = POSITION
    while (M + N < R.usedsize) {
      R.zbranches(M) = R.zbranches(M + N)
      M = M + 1
    }
  }

  def io_shift_right(POSITION: Int, R: znode): Unit = {
    var N: Int = R.usedsize
    while (POSITION < N) {
      R.zbranches(N) = R.zbranches(N - 1)
      N = N - 1
    }
  }

  def io_split(POSITION: Int, R: znode, RL: Ref[znode], RR: Ref[znode], ERR: Ref[error]): Unit = {
    val NUSED: Int = R.usedsize
    val LEFT: Int = POSITION
    val RIGHT: Int = NUSED - POSITION
    val RP: znode = R.parent
    val LEAF: Boolean = R.leaf
    val DIRTY: Boolean = R.dirty
    val R0 = Ref(types.znode.uninit)
    R0.get = types.znode.mkznode(RP, new zbranch_array(BRANCH_SIZE).fill(types.zbranch.uninit), LEAF, DIRTY, RIGHT)
    RR := R0.get
    RL := R
    RL.get.usedsize = LEFT
    var N: Int = 0
    while (N < RIGHT) {
      RR.get.zbranches(N) = RL.get.zbranches(POSITION + N)
      N = N + 1
    }
    RL.get.dirty = true
    RR.get.dirty = true
    ERR := types.error.ESUCCESS
  }

  override def is_log_empty(EMPTY_ : Ref[Boolean]): Unit = {
    apersistence.is_log_empty(EMPTY_)
  }

  override def read_gblock_nodes(LNUM: Int, ADRLIST: address_list, GNDLIST: group_node_list, ERR: Ref[error]): Unit = {
    apersistence.read_gblock_nodes(LNUM, ADRLIST, GNDLIST, ERR)
  }

  override def read_gnd(ADR: address, GND: Ref[group_node], ERR: Ref[error]): Unit = {
    apersistence.read_gnd(ADR, GND, ERR)
  }

  override def recover(MAXINO0: Ref[Int], NL0: nat_list, OS0: nat_set, ERR: Ref[error]): Unit = {
    
    {
      val address_variable0: Ref[address] = Ref[address](ADRT)
      apersistence.recover(address_variable0, MAXINO0, NL0, OS0, ERR)
      ADRT = address_variable0.get
    }
    RT = null
  }

  override def requires_commit(COMMIT_ : Ref[Boolean]): Unit = {
    apersistence.requires_commit(COMMIT_)
  }

  override def set_gblock_refsize(LNUM: Int, N: Int): Unit = {
    apersistence.set_gblock_refsize(LNUM, N)
  }

  override def sync(ERR: Ref[error]): Unit = {
    apersistence.sync(ERR)
  }

  def traverse(MOD: modification, RP: znode, ADR0: address, R: Ref[znode], DIRTY: Ref[Boolean], EXISTS: Ref[Boolean], ADR: Ref[address], ERR: Ref[error]): Unit = {
    io_load(RP, ADR0, R, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (R.get.leaf) {
        val N = Ref[Int](0)
        io_scan(MOD.key, 0, R.get, N)
        val FOUND: Boolean = N.get < R.get.usedsize && R.get.zbranches(N.get).key == MOD.key
        EXISTS := (FOUND && R.get.zbranches(N.get).isInstanceOf[types.zbranch.mkzentry])
        if (EXISTS.get) {
          ADR := R.get.zbranches(N.get).adr
        }
        if (MOD.isInstanceOf[types.modification.contains]) {
          ERR := types.error.ESUCCESS
          DIRTY := false
        } else         if (MOD.isInstanceOf[types.modification.lookup]) {
          ERR := types.error.ESUCCESS
          DIRTY := false
        } else         if (MOD.isInstanceOf[types.modification.store]) {
          if (FOUND != true) {
            io_shift_right(N.get, R.get)
            R.get.usedsize = R.get.usedsize + 1
          }
          R.get.zbranches(N.get) = types.zbranch.mkzentry(MOD.key, MOD.adr)
          DIRTY := true
          ERR := types.error.ESUCCESS
        } else         if (MOD.isInstanceOf[types.modification.remove]) {
          if (FOUND) {
            io_shift_left(N.get, R.get)
            R.get.usedsize = R.get.usedsize - 1
            DIRTY := true
            ERR := types.error.ESUCCESS
          } else {
            DIRTY := false
            ERR := types.error.ESUCCESS
          }
        }
      } else {
        val N = Ref[Int](0)
        io_scan(MOD.key, 1, R.get, N)
        val RC = Ref[znode](R.get.zbranches(N.get).child)
        val ADRC: address = R.get.zbranches(N.get).adr
        traverse(MOD, R.get, ADRC, RC, DIRTY, EXISTS, ADR, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          if (RC.get.usedsize == BRANCH_SIZE) {
            val RR = Ref[znode](null)
            val RL = Ref[znode](null)
            val POSITION: Int = MIN_SIZE
            io_split(POSITION, RC.get, RL, RR, ERR)
            io_shift_right(N.get, R.get)
            R.get.zbranches(N.get) = R.get.zbranches(N.get).updated_child(RL.get)
            R.get.zbranches(N.get) = R.get.zbranches(N.get).updated_key(RL.get.zbranches(POSITION - 1).key)
            R.get.zbranches(N.get + 1) = R.get.zbranches(N.get + 1).updated_child(RR.get)
            R.get.usedsize = R.get.usedsize + 1
            DIRTY := true
          } else           if (RC.get.usedsize == 0 && R.get.usedsize != 0) {
            io_shift_left(N.get, R.get)
            R.get.usedsize = R.get.usedsize - 1
            DIRTY := true
          } else {
            R.get.zbranches(N.get) = R.get.zbranches(N.get).updated_child(RC.get)
          }
        }
      }
      io_dirty(R.get, ADR0, DIRTY.get, ERR)
    } else {
      EXISTS := false
    }
  }

  def traverse_root(MOD: modification, RP: znode, DIRTY: Ref[Boolean], EXISTS: Ref[Boolean], ADR: Ref[address], ERR: Ref[error]): Unit = {
    
    {
      val ref_variable0: Ref[znode] = Ref[znode](RT)
      traverse(MOD, RP, ADRT, ref_variable0, DIRTY, EXISTS, ADR, ERR)
      RT = ref_variable0.get
    }
    if (ERR.get == types.error.ESUCCESS) {
      if (RT.usedsize == BRANCH_SIZE) {
        val RL = Ref[znode](null)
        val RR = Ref[znode](null)
        val POSITION: Int = MIN_SIZE
        io_split(POSITION, RT, RL, RR, ERR)
        val R = Ref(types.znode.uninit)
        R.get = types.znode.mkznode(null, new zbranch_array(BRANCH_SIZE).fill(types.zbranch.uninit), false, true, 2)
        RT = R.get
        DIRTY := true
        val KEY: key = RL.get.zbranches(POSITION - 1).key
        RL.get.parent = RT
        RR.get.parent = RT
        RT.zbranches(0) = types.zbranch.mkzbranch(KEY, ADRT, RL.get)
        RT.zbranches(1) = types.zbranch.mkzbranch(KEY, ADRT, RR.get)
      }
    }
  }

  def truncate(KEY: key, RP: znode, ADR0: address, R: Ref[znode], DIRTY: Ref[Boolean], DONE: Ref[Boolean], AS: address_set, ERR: Ref[error]): Unit = {
    io_load(RP, ADR0, R, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (R.get.leaf) {
        val POSITION = Ref[Int](0)
        io_scan(KEY, 0, R.get, POSITION)
        var N: Int = 0
        while (POSITION.get + N < R.get.usedsize && (KEY.ino == R.get.zbranches(POSITION.get + N).key.ino && R.get.zbranches(POSITION.get + N).key.isInstanceOf[types.key.datakey])) {
          if (R.get.zbranches(POSITION.get + N).isInstanceOf[types.zbranch.mkzentry]) {
            AS += R.get.zbranches(POSITION.get + N).adr
          }
          DIRTY := true
          N = N + 1
        }
        io_shift_n_left(POSITION.get, N, R.get)
        DONE := (POSITION.get + N < R.get.usedsize)
        R.get.usedsize = R.get.usedsize - N
      } else {
        val N = Ref[Int](0)
        io_scan(KEY, 1, R.get, N)
        while (N.get < R.get.usedsize && DONE.get != true) {
          val RC = Ref[znode](R.get.zbranches(N.get).child)
          val ADRC: address = R.get.zbranches(N.get).adr
          truncate(KEY, R.get, ADRC, RC, DIRTY, DONE, AS, ERR)
          R.get.zbranches(N.get) = R.get.zbranches(N.get).updated_child(RC.get)
          if (RC.get.usedsize == 0 && R.get.usedsize != 0) {
            io_shift_left(N.get, R.get)
            R.get.usedsize = R.get.usedsize - 1
          } else {
            N := N.get + 1
          }
        }
      }
    }
    io_dirty(R.get, ADR0, DIRTY.get, ERR)
  }

  override def main_area_LEBs(TOTAL: Ref[Int], FREE: Ref[Int]): Unit = apersistence.main_area_LEBs(TOTAL, FREE)

  override def is_block_eligible_for_gc(N: Int, ISELIGIBLE: Ref[Boolean]): Unit = apersistence.is_block_eligible_for_gc(N, ISELIGIBLE)

  override def compute_stats(TOTAL_BYTES: Ref[Int], FREE_BYTES: Ref[Int], LEB_SIZE: Ref[Int]): Unit = apersistence.compute_stats(TOTAL_BYTES, FREE_BYTES, LEB_SIZE)
}
