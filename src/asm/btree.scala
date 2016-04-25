// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import proc._
import types._
import types.error.error

class btree_asm(var RT : znode, var ADRT : address, val apersistence : apersistence_interface)(implicit _algebraic_implicit: algebraic.Algebraic, _procedures_implicit: proc.Procedures) extends indexpluspersistence_interface {
  import _algebraic_implicit._
  import _procedures_implicit._

  private def btree_commit_rec(R: znode, ADR: Ref[address], LNUM: Ref[Int], ERR: Ref[error]): Unit = {
    if (! R.leaf) {
      var N: Int = 0
      while (N < R.usedsize && ERR.get == types.error.ESUCCESS) {
        val RC: znode = R.zbranches(N).child
        val ADRC = new Ref[address](R.zbranches(N).adr)
        if (RC != null && RC.dirty) {
          btree_commit_rec(RC, ADRC, LNUM, ERR)
          R.zbranches(N) = R.zbranches(N).updated_adr(ADRC.get).deepCopy
        }
        N = N + 1
      }
    }
    if (ERR.get == types.error.ESUCCESS) {
      btree_io_save(R, ADR, LNUM, ERR)
    }
    if (ERR.get == types.error.ESUCCESS)
      R.dirty = false
  }

  private def btree_entries(KEY: key, RP: znode, ADR0: address, R: Ref[znode], DONE: Ref[Boolean], NAMES: stringset, ERR: Ref[error]): Unit = {
    btree_io_load(RP, ADR0, R, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (R.get.leaf) {
        val N = new Ref[Int](0)
        btree_io_scan(KEY, 0, R.get, N)
        while (N.get < R.get.usedsize && (KEY.ino == R.get.zbranches(N.get).key.ino && R.get.zbranches(N.get).key.isInstanceOf[types.key.dentrykey])) {
          NAMES += R.get.zbranches(N.get).key.name
          N := N.get + 1
        }
        DONE := (N.get < R.get.usedsize)
      } else {
        val N = new Ref[Int](0)
        btree_io_scan(KEY, 1, R.get, N)
        while (N.get < R.get.usedsize && DONE.get != true) {
          val ADRC: address = R.get.zbranches(N.get).adr
          val RC = new Ref[znode](R.get.zbranches(N.get).child)
          btree_entries(KEY, R.get, ADRC, RC, DONE, NAMES, ERR)
          N := N.get + 1
        }
      }
    }
  }

  private def btree_io_dirty(R: znode, ADR: address, DIRTY: Boolean, ERR: Ref[error]): Unit = {
    if (DIRTY) {
      if (! R.dirty) {
        val SIZE = new Ref[Int](0)
        apersistence.apersistence_get_iblock_refsize(ADR.lnum, SIZE)
        apersistence.apersistence_set_iblock_refsize(ADR.lnum, SIZE.get - ADR.size)
      }
      R.dirty = true
    }
  }

  def btree_io_load(R: znode, ADR: address, RN: Ref[znode], ERR: Ref[error]): Unit = {
    if (RN.get == null) {
      val IND: index_node = types.index_node.uninit
      apersistence.apersistence_read_ind(ADR, IND, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val R0 : znode = types.znode.mkznode(R, new zbranch_array(BRANCH_SIZE).fill(types.zbranch.uninit), IND.leaf, false, IND.usedsize)
        RN := R0
        var N: Int = 0
        while (N < IND.usedsize) {
          RN.get.zbranches(N) = load(IND.branches(N)).deepCopy
          N = N + 1
        }
      }
    } else
      ERR := types.error.ESUCCESS
  }

  private def btree_io_save(R: znode, ADR: Ref[address], LNUM: Ref[Int], ERR: Ref[error]): Unit = {
    val IND: index_node = types.index_node.indexnode(new branch_array(BRANCH_SIZE).fill(types.branch.uninit), R.leaf, R.usedsize)
    var N: Int = 0
    while (N < R.usedsize) {
      IND.branches(N) = save(R.zbranches(N))
      N = N + 1
    }
    val FD = new Ref[Int](0)
    apersistence.apersistence_get_iblock_size(LNUM.get, FD)
    if (LEB_SIZE <= FD.get + flashsize(IND)) {
      apersistence.apersistence_flush_ind(LNUM.get, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        apersistence.apersistence_allocate_ind(LNUM, ERR)
      }
    } else
      ERR := types.error.ESUCCESS
    if (ERR.get == types.error.ESUCCESS) {
      if (flashsize(IND) <= LEB_SIZE) {
        apersistence.apersistence_add_ind(LNUM.get, IND, ADR, ERR)
        val SIZE = new Ref[Int](0)
        apersistence.apersistence_get_iblock_refsize(LNUM.get, SIZE)
        apersistence.apersistence_set_iblock_refsize(LNUM.get, SIZE.get + ADR.get.size)
      } else
        ERR := types.error.ENOSPC
    }
  }

  private def btree_io_scan(KEY: key, SKIP: Int, R: znode, POS: Ref[Int]): Unit = {
    POS := 0
    while (POS.get + SKIP < R.usedsize && <(R.zbranches(POS.get).key, KEY)) {
      POS := POS.get + 1
    }
  }

  private def btree_io_shift_left(POS: Int, R: znode): Unit = {
    var N: Int = POS
    while (N + 1 < R.usedsize) {
      R.zbranches(N) = R.zbranches(N + 1).deepCopy
      N = N + 1
    }
  }

  private def btree_io_shift_n_left(POS: Int, N: Int, R: znode): Unit = {
    var M: Int = POS
    while (M + N < R.usedsize) {
      R.zbranches(M) = R.zbranches(M + N).deepCopy
      M = M + 1
    }
  }

  private def btree_io_shift_right(POS: Int, R: znode): Unit = {
    var N: Int = R.usedsize
    while (POS < N) {
      R.zbranches(N) = R.zbranches(N - 1).deepCopy
      N = N - 1
    }
  }

  private def btree_io_split(POS: Int, R: znode, RL: Ref[znode], RR: Ref[znode], ERR: Ref[error]): Unit = {
    val NUSED: Int = R.usedsize
    val LEFT: Int = POS
    val RIGHT: Int = NUSED - POS
    val LEAF: Boolean = R.leaf
    val RP: znode = R.parent
    val DIRTY: Boolean = R.dirty
    val R0 : znode = types.znode.mkznode(RP, new zbranch_array(BRANCH_SIZE).fill(types.zbranch.uninit), LEAF, DIRTY, RIGHT)
    RR := R0
    RL := R
    RL.get.usedsize = LEFT
    var N: Int = 0
    while (N < RIGHT) {
      RR.get.zbranches(N) = RL.get.zbranches(POS + N).deepCopy
      N = N + 1
    }
    RL.get.dirty = true
    RR.get.dirty = true
    ERR := types.error.ESUCCESS
  }

  private def btree_traverse(MOD: modification, RP: znode, ADR0: address, R: Ref[znode], DIRTY: Ref[Boolean], EXISTS: Ref[Boolean], ADR: Ref[address], ND: Ref[node], ERR: Ref[error]): Unit = {
    btree_io_load(RP, ADR0, R, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (R.get.leaf) {
        val N = new Ref[Int](0)
        btree_io_scan(MOD.key, 0, R.get, N)
        val FOUND: Boolean = N.get < R.get.usedsize && R.get.zbranches(N.get).key == MOD.key
        EXISTS := (FOUND && R.get.zbranches(N.get).isInstanceOf[types.zbranch.mkzentry])
        if (EXISTS.get)
          ADR := R.get.zbranches(N.get).adr
        
        if (MOD.isInstanceOf[types.modification.contains]) {
          ERR := types.error.ESUCCESS
          DIRTY := false
        } else         if (MOD.isInstanceOf[types.modification.lookup]) {
          ERR := types.error.ESUCCESS
          DIRTY := false
          if (EXISTS.get && R.get.zbranches(N.get).nd == types.node_option.none) {
            val GND = new Ref[group_node](types.group_node.uninit)
            apersistence.apersistence_read_gnd(ADR.get, GND, ERR)
            if (ERR.get == types.error.ESUCCESS) {
              ND := GND.get.nd.deepCopy
              R.get.zbranches(N.get) = R.get.zbranches(N.get).updated_nd(types.node_option.some(ND.get)).deepCopy
            }
          } else           if (EXISTS.get)
            ND := R.get.zbranches(N.get).nd.get.deepCopy
          
        } else         if (MOD.isInstanceOf[types.modification.store]) {
          if (FOUND) {
            R.get.zbranches(N.get) = types.zbranch.mkzentry(MOD.key, MOD.adr, types.node_option.some(MOD.nd)).deepCopy
            DIRTY := true
            ERR := types.error.ESUCCESS
          } else
            assert(false, """abort""")
        } else         if (MOD.isInstanceOf[types.modification.check]) {
          if (FOUND != true) {
            btree_io_shift_right(N.get, R.get)
            R.get.zbranches(N.get) = types.zbranch.mkzchecked(MOD.key)
            R.get.usedsize = R.get.usedsize + 1
            DIRTY := true
            ERR := types.error.ESUCCESS
          } else {
            DIRTY := false
            ERR := types.error.ESUCCESS
          }
        } else         if (MOD.isInstanceOf[types.modification.remove]) {
          if (FOUND) {
            btree_io_shift_left(N.get, R.get)
            R.get.usedsize = R.get.usedsize - 1
            DIRTY := true
            ERR := types.error.ESUCCESS
          } else {
            DIRTY := false
            ERR := types.error.ESUCCESS
          }
        }
      } else {
        val N = new Ref[Int](0)
        btree_io_scan(MOD.key, 1, R.get, N)
        val RC = new Ref[znode](R.get.zbranches(N.get).child)
        val ADRC: address = R.get.zbranches(N.get).adr
        btree_traverse(MOD, R.get, ADRC, RC, DIRTY, EXISTS, ADR, ND, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          if (RC.get.usedsize == BRANCH_SIZE) {
            val RR = new Ref[znode](null)
            val RL = new Ref[znode](null)
            val POS: Int = MIN_SIZE
            btree_io_split(POS, RC.get, RL, RR, ERR)
            btree_io_shift_right(N.get, R.get)
            R.get.zbranches(N.get) = R.get.zbranches(N.get).updated_child(RL.get).deepCopy
            R.get.zbranches(N.get) = R.get.zbranches(N.get).updated_key(RL.get.zbranches(POS - 1).key).deepCopy
            R.get.zbranches(N.get + 1) = R.get.zbranches(N.get + 1).updated_child(RR.get).deepCopy
            R.get.usedsize = R.get.usedsize + 1
            DIRTY := true
          } else           if (RC.get.usedsize == 0 && R.get.usedsize != 0) {
            btree_io_shift_left(N.get, R.get)
            R.get.usedsize = R.get.usedsize - 1
            DIRTY := true
          } else
            R.get.zbranches(N.get) = R.get.zbranches(N.get).updated_child(RC.get).deepCopy
        }
      }
      btree_io_dirty(R.get, ADR0, DIRTY.get, ERR)
    }
  }

  private def btree_traverse_root(MOD: modification, RP: znode, DIRTY: Ref[Boolean], EXISTS: Ref[Boolean], ADR: Ref[address], ND: Ref[node], ERR: Ref[error]): Unit = {
    val rp: Ref[znode] = new Ref[znode](RT)
    btree_traverse(MOD, RP, ADRT, rp, DIRTY, EXISTS, ADR, ND, ERR)
    RT = rp.get
    if (ERR.get == types.error.ESUCCESS) {
      if (RT.usedsize == BRANCH_SIZE) {
        val POS: Int = MIN_SIZE
        val RL = new Ref[znode](null)
        val RR = new Ref[znode](null)
        btree_io_split(POS, RT, RL, RR, ERR)
        val R : znode = types.znode.mkznode(null, new zbranch_array(BRANCH_SIZE).fill(types.zbranch.uninit), false, true, 2)
        RT = R
        DIRTY := true
        val KEY: key = RL.get.zbranches(POS - 1).key
        RL.get.parent = RT
        RR.get.parent = RT
        RT.zbranches(0) = types.zbranch.mkzbranch(KEY, ADRT, RL.get)
        RT.zbranches(1) = types.zbranch.mkzbranch(KEY, ADRT, RR.get)
      }
    }
  }

  private def btree_truncate(KEY: key, RP: znode, ADR0: address, R: Ref[znode], DIRTY: Ref[Boolean], DONE: Ref[Boolean], AS: address_set, ERR: Ref[error]): Unit = {
    btree_io_load(RP, ADR0, R, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (R.get.leaf) {
        val POS = new Ref[Int](0)
        btree_io_scan(KEY, 0, R.get, POS)
        var N: Int = 0
        while (POS.get + N < R.get.usedsize && (KEY.ino == R.get.zbranches(POS.get + N).key.ino && R.get.zbranches(POS.get + N).key.isInstanceOf[types.key.datakey])) {
          if (R.get.zbranches(POS.get + N).isInstanceOf[types.zbranch.mkzentry])
            AS += R.get.zbranches(POS.get + N).adr
          
          DIRTY := true
          N = N + 1
        }
        btree_io_shift_n_left(POS.get, N, R.get)
        DONE := (POS.get + N < R.get.usedsize)
        R.get.usedsize = R.get.usedsize - N
      } else {
        val N = new Ref[Int](0)
        btree_io_scan(KEY, 1, R.get, N)
        while (N.get < R.get.usedsize && DONE.get != true) {
          val ADRC: address = R.get.zbranches(N.get).adr
          val RC = new Ref[znode](R.get.zbranches(N.get).child)
          val dirty = DIRTY.get
          btree_truncate(KEY, R.get, ADRC, RC, DIRTY, DONE, AS, ERR)
          R.get.zbranches(N.get) = R.get.zbranches(N.get).updated_child(RC.get).deepCopy
          DIRTY := DIRTY.get || dirty
          if (RC.get.usedsize == 0 && R.get.usedsize != 0) {
            btree_io_shift_left(N.get, R.get)
            R.get.usedsize = R.get.usedsize - 1
          } else
            N := N.get + 1
        }
      }
    }
    btree_io_dirty(R.get, ADR0, DIRTY.get, ERR)
  }

  override def indexpluspersistence_add_gnd(LNUM: Int, GND: group_node, ADR: Ref[address], ERR: Ref[error]): Unit = {
    apersistence.apersistence_add_gnd(LNUM, GND, ADR, ERR)
  }

  override def indexpluspersistence_add_gnds(LNUM: Int, GNDLIST: group_node_list, ADRLIST: address_list, ERR: Ref[error]): Unit = {
    apersistence.apersistence_add_gnds(LNUM, GNDLIST, ADRLIST, ERR)
  }

  override def indexpluspersistence_allocate_gnd(N: Ref[Int], ERR: Ref[error]): Unit = {
    apersistence.apersistence_allocate_gnd(N, ERR)
  }

  override def indexpluspersistence_commit(MAXINO0: Int, OS0: nat_set, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val ADR = new Ref[address](ADRT)
    if (RT != null && RT.dirty) {
      val LNUM = new Ref[Int](0)
      apersistence.apersistence_allocate_ind(LNUM, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        btree_commit_rec(RT, ADR, LNUM, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        apersistence.apersistence_flush_ind(LNUM.get, ERR)
      }
    }
    if (ERR.get == types.error.ESUCCESS) {
      apersistence.apersistence_commit(ADR.get, MAXINO0, OS0, ERR)
    }
    if (ERR.get == types.error.ESUCCESS)
      ADRT = ADR.get
  }

  override def indexpluspersistence_deallocate_gnd(N: Int, ERR: Ref[error]): Unit = {
    apersistence.apersistence_deallocate_gnd(N, ERR)
  }

  override def indexpluspersistence_flush_gnd(LNUM: Int, ERR: Ref[error]): Unit = {
    apersistence.apersistence_flush_gnd(LNUM, ERR)
  }

  override def indexpluspersistence_format(VOLSIZE: Int, MAXINO0: Int, ERR: Ref[error]): Unit = {
    RT = null
    apersistence.apersistence_format(VOLSIZE, MAXINO0, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val LNUM = new Ref[Int](0)
      val IND: index_node = types.index_node.indexnode(new branch_array(BRANCH_SIZE).fill(types.branch.uninit), true, 0)
      val SIZE: Int = flashsize(IND)
      if (SIZE > LEB_SIZE) {
        debug("btree: initial index node to large")
        ERR := types.error.ENOSPC
      }
      if (ERR.get == types.error.ESUCCESS) {
        apersistence.apersistence_allocate_ind(LNUM, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        val adr: Ref[address] = new Ref[address](ADRT)
        apersistence.apersistence_add_ind(LNUM.get, IND, adr, ERR)
        ADRT = adr.get
      }
      if (ERR.get == types.error.ESUCCESS) {
        apersistence.apersistence_set_iblock_refsize(LNUM.get, SIZE)
      }
      if (ERR.get == types.error.ESUCCESS) {
        apersistence.apersistence_flush_ind(LNUM.get, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        val NS: nat_set = new nat_set()
        apersistence.apersistence_commit(ADRT, MAXINO0, NS, ERR)
      }
    }
  }

  override def indexpluspersistence_get_gblock_refsize(LNUM: Int, N: Ref[Int]): Unit = {
    apersistence.apersistence_get_gblock_refsize(LNUM, N)
  }

  override def indexpluspersistence_get_gblock_size(LNUM: Int, N: Ref[Int]): Unit = {
    apersistence.apersistence_get_gblock_size(LNUM, N)
  }

  override def indexpluspersistence_get_gc_block(N: Ref[Int], ERR: Ref[error]): Unit = {
    apersistence.apersistence_get_gc_block(N, ERR)
  }

  override def indexpluspersistence_index_checkdata(KEY: key, N: Int, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
  }

  override def indexpluspersistence_index_checkkey(KEY: key, EXISTS: Ref[Boolean], ERR: Ref[error]): Unit = {
    val OLDND = new Ref[node](types.node.uninit)
    val OLDADR = new Ref[address](types.address.uninit)
    val DIRTY = new Ref[Boolean](helpers.scala.Boolean.uninit)
    btree_traverse_root(types.modification.check(KEY), null, DIRTY, EXISTS, OLDADR, OLDND, ERR)
  }

  override def indexpluspersistence_index_contains(KEY: key, EXISTS: Ref[Boolean], ERR: Ref[error]): Unit = {
    val ND = new Ref[node](types.node.uninit)
    val ADR = new Ref[address](types.address.uninit)
    val DIRTY = new Ref[Boolean](helpers.scala.Boolean.uninit)
    btree_traverse_root(types.modification.contains(KEY), null, DIRTY, EXISTS, ADR, ND, ERR)
  }

  override def indexpluspersistence_index_entries(KEY: key, NAMES: stringset, ERR: Ref[error]): Unit = {
    val DONE = new Ref[Boolean](false)
    val rp: Ref[znode] = new Ref[znode](RT)
    btree_entries(types.key.dentrykey(KEY.ino, ""), null, ADRT, rp, DONE, NAMES, ERR)
    RT = rp.get
  }

  override def indexpluspersistence_index_lookup(KEY: key, EXISTS: Ref[Boolean], ADR: Ref[address], ND: Ref[node], ERR: Ref[error]): Unit = {
    val DIRTY = new Ref[Boolean](helpers.scala.Boolean.uninit)
    btree_traverse_root(types.modification.lookup(KEY), null, DIRTY, EXISTS, ADR, ND, ERR)
  }

  override def indexpluspersistence_index_remove(KEY: key, OLDADR: Ref[address], EXISTS: Ref[Boolean]): Unit = {
    val ERR = new Ref[error](types.error.uninit)
    val OLDND = new Ref[node](types.node.uninit)
    val DIRTY = new Ref[Boolean](helpers.scala.Boolean.uninit)
    btree_traverse_root(types.modification.remove(KEY), null, DIRTY, EXISTS, OLDADR, OLDND, ERR)
  }

  override def indexpluspersistence_index_store(KEY: key, ADR: address, ND: node, OLDADR: Ref[address], EXISTS: Ref[Boolean], ERR: Ref[error]): Unit = {
    val OLDND = new Ref[node](types.node.uninit)
    val DIRTY = new Ref[Boolean](helpers.scala.Boolean.uninit)
    btree_traverse_root(types.modification.store(KEY, ADR, ND), null, DIRTY, EXISTS, OLDADR, OLDND, ERR)
  }

  override def indexpluspersistence_index_truncate(KEY: key, N: Int, AS: address_set, ERR: Ref[error]): Unit = {
    val DONE = new Ref[Boolean](false)
    val DIRTY = new Ref[Boolean](false)
    val rp: Ref[znode] = new Ref[znode](RT)
    btree_truncate(types.key.datakey(KEY.ino, ((N + VFS_PAGE_SIZE) - 1) / VFS_PAGE_SIZE), null, ADRT, rp, DIRTY, DONE, AS, ERR)
    RT = rp.get
  }

  override def indexpluspersistence_read_gblock_nodes(LNUM: Int, ADRLIST: address_list, GNDLIST: group_node_list, ERR: Ref[error]): Unit = {
    apersistence.apersistence_read_gblock_nodes(LNUM, ADRLIST, GNDLIST, ERR)
  }

  override def indexpluspersistence_read_gnd(ADR: address, GND: Ref[group_node], ERR: Ref[error]): Unit = {
    apersistence.apersistence_read_gnd(ADR, GND, ERR)
  }

  override def indexpluspersistence_recover(MAXINO0: Ref[Int], NL0: nat_list, OS0: nat_set, ERR: Ref[error]): Unit = {
    val adr: Ref[address] = new Ref[address](ADRT)
    apersistence.apersistence_recover(adr, MAXINO0, NL0, OS0, ERR)
    ADRT = adr.get
    RT = null
  }

  override def indexpluspersistence_requires_commit(COMMIT_ : Ref[Boolean]): Unit = {
    apersistence.apersistence_requires_commit(COMMIT_)
  }

  override def indexpluspersistence_set_gblock_refsize(LNUM: Int, N: Int): Unit = {
    apersistence.apersistence_set_gblock_refsize(LNUM, N)
  }

}

