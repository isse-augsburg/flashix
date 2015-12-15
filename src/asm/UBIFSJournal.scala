package asm

import helpers.scala._
import types._
import types.error.error
import debug.DebugUBIFSJournal

class UBIFSJournal(var JNLHEADVALID: Boolean, var ADRT: address, val RO: key_set, var MAXINO: Int, var SQNUM: Int, var JNLHEAD: Int, var RT: znode, val persistence: AbstractPersistence)(implicit _algebraic_implicit: algebraic.Algebraic)
  extends AUBIFSJournal with DebugUBIFSJournal {
  import _algebraic_implicit._

  override def aubifs_commit(ERR: Ref[error]): Unit = {
    println("ubifs: attempting commit")
    val ADR = new Ref[address](ADRT)
    btree_commit(RT, ADR, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      persistence.persistence_write_superblock(ADR.get, MAXINO, RO, ERR)
      JNLHEADVALID = false
      if (ERR.get == types.error.ESUCCESS)
        ADRT = ADR.get

    }
    if (ERR.get != types.error.ESUCCESS && RT != null)
      RT.dirty = true

    if (ERR.get == types.error.ESUCCESS)
      println("ubifs: commit successfull")
    else println("ubifs: commit failed" + ERR.get)
  }

  override def aubifs_internal_check_commit(ERR: Ref[error]): Unit = {
    val COMMIT_ = new Ref[Boolean](false)
    persistence.persistence_requires_commit(COMMIT_)
    if (COMMIT_.get) {
      aubifs_commit(ERR)
    }
  }

  override def aubifs_internal_format(VOLSIZE: Int, ERR: Ref[error]): Unit = {
    JNLHEADVALID = false
    persistence.persistence_format(VOLSIZE, ERR)
    val ADRT0: Ref[address] = new Ref[address](ADRT)
    persistence.persistence_get_root(ADRT0)
    ADRT = ADRT0.get
    val MAXINO1: Ref[Int] = new Ref[Int](MAXINO)
    persistence.persistence_get_maxino(MAXINO1)
    MAXINO = MAXINO1.get
    RT = null
    RO.clear
  }

  override def aubifs_readflash(LOG: address_list, KS: key_set, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    JNLHEAD = 0
    JNLHEADVALID = false
    RT = null
    val MAXINO1: Ref[Int] = new Ref[Int](MAXINO)
    persistence.persistence_get_maxino(MAXINO1)
    MAXINO = MAXINO1.get
    val ADRT0: Ref[address] = new Ref[address](ADRT)
    persistence.persistence_get_root(ADRT0)
    ADRT = ADRT0.get
    persistence.persistence_read_orph(RO, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      ubifs_restore_log(LOG, ERR)
    }
    KS := RO.deepCopy
  }

  def btree_commit(R: znode, ADR: Ref[address], ERR: Ref[error]): Unit = {
    if (R != null && R.dirty) {
      val LNUM = new Ref[Int](0)
      persistence.persistence_allocate_ind(LNUM, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        btree_commit_rec(R, ADR, LNUM, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        persistence.persistence_flush_ind(LNUM.get, ERR)
      }
      if (ERR.get != types.error.ESUCCESS) {
        btree_io_dirty(R, ADR.get, true, ERR)
      }
    } else
      ERR := types.error.ESUCCESS
  }

  def btree_commit_rec(R: znode, ADR: Ref[address], LNUM: Ref[Int], ERR: Ref[error]): Unit = {
    if (!R.leaf) {
      var N: Int = 0
      while (N < R.usedsize && ERR.get == types.error.ESUCCESS) {
        val ADRC = new Ref[address](R.zbranches(N).adr)
        val RC: znode = R.zbranches(N).child
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

  def btree_entries(KEY: key, RP: znode, ADRT: address, RT: Ref[znode], DONE: Ref[Boolean], NAMES: stringset, ERR: Ref[error]): Unit = {
    btree_io_load(RP, ADRT, RT, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (RT.get.leaf) {
        btree_entries_leaf(KEY, RT.get, DONE, NAMES, ERR)
      } else {
        btree_entries_node(KEY, RT.get, DONE, NAMES, ERR)
      }
    }
  }

  def btree_entries_leaf(KEY: key, R: znode, DONE: Ref[Boolean], NAMES: stringset, ERR: Ref[error]): Unit = {
    val N = new Ref[Int](0)
    btree_io_scan(KEY, 0, R, N)
    while (N.get < R.usedsize && (KEY.ino == R.zbranches(N.get).key.ino && R.zbranches(N.get).key.isInstanceOf[types.key.dentrykey])) {
      NAMES += R.zbranches(N.get).key.name
      N := N.get + 1
    }
    DONE := (N.get < R.usedsize)
  }

  def btree_entries_node(KEY: key, R: znode, DONE: Ref[Boolean], NAMES: stringset, ERR: Ref[error]): Unit = {
    val N = new Ref[Int](0)
    btree_io_scan(KEY, 1, R, N)
    while (N.get < R.usedsize && DONE.get != true) {
      val RC = new Ref[znode](R.zbranches(N.get).child)
      val ADRC: address = R.zbranches(N.get).adr
      btree_entries(KEY, R, ADRC, RC, DONE, NAMES, ERR)
      N := N.get + 1
    }
  }

  def btree_io_dirty(R: znode, ADR: address, DIRTY: Boolean, ERR: Ref[error]): Unit = {
    if (DIRTY) {
      if (!R.dirty) {
        val SIZE = new Ref[Int](0)
        persistence.persistence_get_iblock_refsize(ADR.lnum, SIZE)
        persistence.persistence_set_iblock_refsize(ADR.lnum, SIZE.get - ADR.size)
      }
      R.dirty = true
    }
  }

  def btree_io_load(R: znode, ADR: address, RN: Ref[znode], ERR: Ref[error]): Unit = {
    if (RN.get == null) {
      val IND = new Ref[index_node](index_node.uninit)
      persistence.persistence_read_ind(ADR, IND, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val R0: znode = types.znode.mkznode(R, new zbranch_array(BRANCH_SIZE).fill(zbranch.uninit), IND.get.leaf, false, IND.get.usedsize)
        RN := R0
        var N: Int = 0
        while (N < IND.get.usedsize) {
          RN.get.zbranches(N) = load(IND.get.branches(N)).deepCopy
          N = N + 1
        }
      }
    } else
      ERR := types.error.ESUCCESS
  }

  def btree_io_save(R: znode, ADR: Ref[address], LNUM: Ref[Int], ERR: Ref[error]): Unit = {
    var IND: index_node = types.index_node.indexnode(new branch_array(BRANCH_SIZE).fill(branch.uninit), R.leaf, R.usedsize)
    var N: Int = 0
    while (N < R.usedsize) {
      IND.branches(N) = save(R.zbranches(N))
      N = N + 1
    }
    val SIZE0 = new Ref[Int](0)
    persistence.persistence_get_iblock_size(LNUM.get, SIZE0)
    if (LEB_SIZE <= SIZE0.get + flashsize(IND)) {
      persistence.persistence_flush_ind(LNUM.get, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        persistence.persistence_allocate_ind(LNUM, ERR)
      }
    } else
      ERR := types.error.ESUCCESS
    if (ERR.get == types.error.ESUCCESS) {
      if (flashsize(IND) <= LEB_SIZE) {
        persistence.persistence_add_ind(LNUM.get, IND, ADR, ERR)
        val SIZE = new Ref[Int](0)
        persistence.persistence_get_iblock_refsize(LNUM.get, SIZE)
        persistence.persistence_set_iblock_refsize(LNUM.get, SIZE.get + ADR.get.size)
      } else
        ERR := types.error.ENOSPC
    }
  }

  def btree_io_scan(KEY: key, SKIP: Int, R: znode, POS: Ref[Int]): Unit = {
    POS := 0
    while (POS.get + SKIP < R.usedsize && <(R.zbranches(POS.get).key, KEY)) {
      POS := POS.get + 1
    }
  }

  def btree_io_shift_left(POS: Int, R: znode): Unit = {
    var N: Int = POS
    while (N + 1 < R.usedsize) {
      R.zbranches(N) = R.zbranches(N + 1).deepCopy
      N = N + 1
    }
  }

  def btree_io_shift_n_left(POS: Int, N: Int, R: znode): Unit = {
    var M: Int = POS
    while (M + N < R.usedsize) {
      R.zbranches(M) = R.zbranches(M + N).deepCopy
      M = M + 1
    }
  }

  def btree_io_shift_right(POS: Int, R: znode): Unit = {
    var N: Int = R.usedsize
    while (POS < N) {
      R.zbranches(N) = R.zbranches(N - 1).deepCopy
      N = N - 1
    }
  }

  def btree_io_split(POS: Int, R: znode, RL: Ref[znode], RR: Ref[znode], ERR: Ref[error]): Unit = {
    val NUSED: Int = R.usedsize
    val LEFT: Int = POS
    val RIGHT: Int = NUSED - POS
    val DIRTY: Boolean = R.dirty
    val RP: znode = R.parent
    val LEAF: Boolean = R.leaf
    val R0: znode = types.znode.mkznode(RP, new zbranch_array(BRANCH_SIZE).fill(zbranch.uninit), LEAF, DIRTY, RIGHT)
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

  def btree_traverse(MOD: modification, RP: znode, ADRT: address, RT: Ref[znode], DIRTY: Ref[Boolean], EXISTS: Ref[Boolean], ADR: Ref[address], ND: Ref[node], ERR: Ref[error]): Unit = {
    btree_io_load(RP, ADRT, RT, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (RT.get.leaf) {
        btree_traverse_leaf(MOD, RT.get, DIRTY, EXISTS, ADR, ND, ERR)
      } else {
        btree_traverse_node(MOD, RT.get, DIRTY, EXISTS, ADR, ND, ERR)
      }
      btree_io_dirty(RT.get, ADRT, DIRTY.get, ERR)
    }
  }

  def btree_traverse_leaf(MOD: modification, R: znode, DIRTY: Ref[Boolean], EXISTS: Ref[Boolean], ADR: Ref[address], ND: Ref[node], ERR: Ref[error]): Unit = {
    val N = new Ref[Int](0)
    btree_io_scan(MOD.key, 0, R, N)
    val FOUND: Boolean = N.get < R.usedsize && R.zbranches(N.get).key == MOD.key
    EXISTS := (FOUND && R.zbranches(N.get).isInstanceOf[types.zbranch.mkzentry])
    if (EXISTS.get)
      ADR := R.zbranches(N.get).adr

    if (MOD.isInstanceOf[types.modification.contains]) {
      ERR := types.error.ESUCCESS
      DIRTY := false
    } else {
      if (MOD.isInstanceOf[types.modification.lookup]) {
        ERR := types.error.ESUCCESS
        DIRTY := false
        if (EXISTS.get && R.zbranches(N.get).nd == types.node_option.none) {
          val GND = new Ref[group_node](group_node.uninit)
          persistence.persistence_read_gnd(ADR.get, GND, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            ND := GND.get.nd.deepCopy
            R.zbranches(N.get) = R.zbranches(N.get).updated_nd(types.node_option.some(ND.get)).deepCopy
          }
        } else {
          if (EXISTS.get)
            ND := R.zbranches(N.get).nd.get.deepCopy

        }
      } else {
        if (MOD.isInstanceOf[types.modification.store]) {
          if (FOUND) {
            R.zbranches(N.get) = types.zbranch.mkzentry(MOD.key, MOD.adr, types.node_option.some(MOD.nd)).deepCopy
            DIRTY := true
            ERR := types.error.ESUCCESS
          } else
            assert(false)
        } else {
          if (MOD.isInstanceOf[types.modification.check]) {
            if (FOUND != true) {
              btree_io_shift_right(N.get, R)
              R.zbranches(N.get) = types.zbranch.mkzchecked(MOD.key)
              R.usedsize = R.usedsize + 1
              DIRTY := true
              ERR := types.error.ESUCCESS
            } else {
              DIRTY := false
              ERR := types.error.ESUCCESS
            }
          } else {
            if (MOD.isInstanceOf[types.modification.remove]) {
              if (FOUND) {
                btree_io_shift_left(N.get, R)
                R.usedsize = R.usedsize - 1
                DIRTY := true
                ERR := types.error.ESUCCESS
              } else {
                DIRTY := false
                ERR := types.error.ESUCCESS
              }
            }
          }
        }
      }
    }
  }

  def btree_traverse_node(MOD: modification, RT: znode, DIRTY: Ref[Boolean], EXISTS: Ref[Boolean], ADR: Ref[address], ND: Ref[node], ERR: Ref[error]): Unit = {
    val N = new Ref[Int](0)
    btree_io_scan(MOD.key, 1, RT, N)
    val RC = new Ref[znode](RT.zbranches(N.get).child)
    val ADRC: address = RT.zbranches(N.get).adr
    btree_traverse(MOD, RT, ADRC, RC, DIRTY, EXISTS, ADR, ND, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (RC.get.usedsize == BRANCH_SIZE) {
        val RR = new Ref[znode](null)
        val RL = new Ref[znode](null)
        val POS: Int = MIN_SIZE
        btree_io_split(POS, RC.get, RL, RR, ERR)
        btree_io_shift_right(N.get, RT)
        RT.zbranches(N.get) = RT.zbranches(N.get).updated_child(RL.get).deepCopy
        RT.zbranches(N.get) = RT.zbranches(N.get).updated_key(RL.get.zbranches(POS - 1).key).deepCopy
        RT.zbranches(N.get + 1) = RT.zbranches(N.get + 1).updated_child(RR.get).deepCopy
        RT.usedsize = RT.usedsize + 1
        DIRTY := true
      } else {
        if (RC.get.usedsize == 0 && RT.usedsize != 0) {
          btree_io_shift_left(N.get, RT)
          RT.usedsize = RT.usedsize - 1
          DIRTY := true
        } else
          RT.zbranches(N.get) = RT.zbranches(N.get).updated_child(RC.get).deepCopy
      }
    }
  }

  def btree_traverse_root(MOD: modification, RP: znode, DIRTY: Ref[Boolean], EXISTS: Ref[Boolean], ADR: Ref[address], ND: Ref[node], ERR: Ref[error]): Unit = {
    val RT0: Ref[znode] = new Ref[znode](RT)
    btree_traverse(MOD, RP, ADRT, RT0, DIRTY, EXISTS, ADR, ND, ERR)
    RT = RT0.get
    if (ERR.get == types.error.ESUCCESS) {
      if (RT.usedsize == BRANCH_SIZE) {
        val POS: Int = MIN_SIZE
        val RL = new Ref[znode](null)
        val RR = new Ref[znode](null)
        btree_io_split(POS, RT, RL, RR, ERR)
        val R: znode = types.znode.mkznode(null, new zbranch_array(BRANCH_SIZE).fill(zbranch.uninit), false, true, 2)
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

  def btree_truncate(KEY: key, RP: znode, ADRT: address, RT: Ref[znode], DIRTY: Ref[Boolean], DONE: Ref[Boolean], AX: address_list, ERR: Ref[error]): Unit = {
    btree_io_load(RP, ADRT, RT, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (RT.get.leaf) {
        btree_truncate_leaf(KEY, RT.get, DIRTY, DONE, AX, ERR)
      } else {
        btree_truncate_node(KEY, RT.get, DIRTY, DONE, AX, ERR)
      }
    }
    btree_io_dirty(RT.get, ADRT, DIRTY.get, ERR)
  }

  def btree_truncate_leaf(KEY: key, R: znode, DIRTY: Ref[Boolean], DONE: Ref[Boolean], AX: address_list, ERR: Ref[error]): Unit = {
    val POS = new Ref[Int](0)
    btree_io_scan(KEY, 0, R, POS)
    var N: Int = 0
    while (POS.get + N < R.usedsize && (KEY.ino == R.zbranches(POS.get + N).key.ino && R.zbranches(POS.get + N).key.isInstanceOf[types.key.datakey])) {
      if (R.zbranches(POS.get + N).isInstanceOf[types.zbranch.mkzentry])
        AX += R.zbranches(POS.get + N).adr

      DIRTY := true
      N = N + 1
    }
    btree_io_shift_n_left(POS.get, N, R)
    DONE := (POS.get + N < R.usedsize)
    R.usedsize = R.usedsize - N
  }

  def btree_truncate_node(KEY: key, R: znode, DIRTY: Ref[Boolean], DONE: Ref[Boolean], AX: address_list, ERR: Ref[error]): Unit = {
    val N = new Ref[Int](0)
    btree_io_scan(KEY, 1, R, N)
    while (N.get < R.usedsize && DONE.get != true) {
      val ADRC: address = R.zbranches(N.get).adr
      val RC = new Ref[znode](R.zbranches(N.get).child)
      btree_truncate(KEY, R, ADRC, RC, DIRTY, DONE, AX, ERR)
      if (RC.get.usedsize == 0 && R.usedsize != 0) {
        btree_io_shift_left(N.get, R)
        R.usedsize = R.usedsize - 1
      } else
        N := N.get + 1
    }
  }

  def gjournal_allocate(SIZE: Int, ERR: Ref[error]): Unit = {
    if (SIZE > LEB_SIZE)
      ERR := types.error.ENOSPC
    else {
      val N = new Ref[Int](0)
      if (JNLHEADVALID) {
        persistence.persistence_get_gblock_size(JNLHEAD, N)
      }
      if (JNLHEADVALID && N.get + SIZE <= LEB_SIZE)
        ERR := types.error.ESUCCESS
      else {
        ERR := types.error.ESUCCESS
        if (JNLHEADVALID) {
          persistence.persistence_flush_gnd(JNLHEAD, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          persistence.persistence_allocate_gnd(N, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          JNLHEADVALID = true
          JNLHEAD = N.get
        } else
          JNLHEADVALID = false
      }
    }
  }

  override def index_checkdata(KEY: key, N: Int, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
  }

  override def index_checkkey(KEY: key, EXISTS: Ref[Boolean], ERR: Ref[error]): Unit = {
    val OLDND = new Ref[node](node.uninit)
    val OLDADR = new Ref[address](address.uninit)
    val DIRTY = new Ref[Boolean](helpers.scala.Boolean.uninit)
    btree_traverse_root(types.modification.check(KEY), null, DIRTY, EXISTS, OLDADR, OLDND, ERR)
  }

  override def index_contains(KEY: key, EXISTS: Ref[Boolean], ERR: Ref[error]): Unit = {
    val ND = new Ref[node](node.uninit)
    val ADR = new Ref[address](address.uninit)
    val DIRTY = new Ref[Boolean](helpers.scala.Boolean.uninit)
    btree_traverse_root(types.modification.contains(KEY), null, DIRTY, EXISTS, ADR, ND, ERR)
  }

  override def index_entries(KEY: key, NAMES: stringset, ERR: Ref[error]): Unit = {
    val DONE = new Ref[Boolean](false)
    val RT0: Ref[znode] = new Ref[znode](RT)
    btree_entries(types.key.dentrykey(KEY.ino, ""), null, ADRT, RT0, DONE, NAMES, ERR)
    RT = RT0.get
  }

  override def index_lookup(KEY: key, EXISTS: Ref[Boolean], ND: Ref[node], ERR: Ref[error]): Unit = {
    val ADR = new Ref[address](address.uninit)
    val DIRTY = new Ref[Boolean](helpers.scala.Boolean.uninit)
    btree_traverse_root(types.modification.lookup(KEY), null, DIRTY, EXISTS, ADR, ND, ERR)
  }

  override def index_newino(KEY: Ref[key], ERR: Ref[error]): Unit = {
    KEY := types.key.inodekey(MAXINO)
    MAXINO = MAXINO + 1
    ERR := types.error.ESUCCESS
  }

  override def index_remove(KEY: key): Unit = {
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    val OLDADR = new Ref[address](address.uninit)
    val ERR = new Ref[error](error.uninit)
    val OLDND = new Ref[node](node.uninit)
    val DIRTY = new Ref[Boolean](helpers.scala.Boolean.uninit)
    btree_traverse_root(types.modification.remove(KEY), null, DIRTY, EXISTS, OLDADR, OLDND, ERR)
    if (ERR.get == types.error.ESUCCESS && EXISTS.get) {
      val N = new Ref[Int](0)
      persistence.persistence_get_gblock_refsize(OLDADR.get.lnum, N)
      persistence.persistence_set_gblock_refsize(OLDADR.get.lnum, N.get - OLDADR.get.size)
    }
  }

  override def index_store(KEY: key, ADR: address, ND: node, ERR: Ref[error]): Unit = {
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    val OLDADR = new Ref[address](address.uninit)
    val OLDND = new Ref[node](node.uninit)
    val DIRTY = new Ref[Boolean](helpers.scala.Boolean.uninit)
    btree_traverse_root(types.modification.store(KEY, ADR, ND), null, DIRTY, EXISTS, OLDADR, OLDND, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val N = new Ref[Int](0)
      if (EXISTS.get) {
        persistence.persistence_get_gblock_refsize(OLDADR.get.lnum, N)
        persistence.persistence_set_gblock_refsize(OLDADR.get.lnum, N.get - OLDADR.get.size)
      }
      persistence.persistence_get_gblock_refsize(ADR.lnum, N)
      persistence.persistence_set_gblock_refsize(ADR.lnum, N.get + ADR.size)
      MAXINO = max(KEY.ino + 1, MAXINO)
    }
  }

  override def index_truncate(KEY: key, N: Int, ERR: Ref[error]): Unit = {
    val AX: address_list = new address_list()
    val DONE = new Ref[Boolean](false)
    val DIRTY = new Ref[Boolean](false)
    val RT0: Ref[znode] = new Ref[znode](RT)
    btree_truncate(types.key.datakey(KEY.ino, ((N + VFS_PAGE_SIZE) - 1) / VFS_PAGE_SIZE), null, ADRT, RT0, DIRTY, DONE, AX, ERR)
    RT = RT0.get
    remove_addresses(AX)
  }

  override def journal_add1(ND0: node, ADR0: Ref[address], ERR: Ref[error]): Unit = {
    val SIZE0: Int = flashsize(ND0)
    gjournal_allocate(SIZE0, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      persistence.persistence_add_gnd(JNLHEAD, types.group_node.mkgnode(ND0, SQNUM, true, true), ADR0, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      persistence.persistence_flush_gnd(JNLHEAD, ERR)
    }
    if (ERR.get == types.error.ESUCCESS)
      SQNUM = SQNUM + 1
    else
      JNLHEADVALID = false
  }

  override def journal_add2(ND0: node, ND1: node, ADR0: Ref[address], ADR1: Ref[address], ERR: Ref[error]): Unit = {
    val SIZE1: Int = flashsize(ND1)
    val SIZE0: Int = flashsize(ND0)
    gjournal_allocate(SIZE0 + SIZE1, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      persistence.persistence_add_gnd(JNLHEAD, types.group_node.mkgnode(ND0, SQNUM, true, false), ADR0, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      persistence.persistence_add_gnd(JNLHEAD, types.group_node.mkgnode(ND1, SQNUM + 1, false, true), ADR1, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      persistence.persistence_flush_gnd(JNLHEAD, ERR)
    }
    if (ERR.get == types.error.ESUCCESS)
      SQNUM = SQNUM + 2
    else
      JNLHEADVALID = false
  }

  override def journal_add3(ND0: node, ND1: node, ND2: node, ADR0: Ref[address], ADR1: Ref[address], ADR2: Ref[address], ERR: Ref[error]): Unit = {
    val SIZE2: Int = flashsize(ND2)
    val SIZE1: Int = flashsize(ND1)
    val SIZE0: Int = flashsize(ND0)
    gjournal_allocate(SIZE0 + (SIZE1 + SIZE2), ERR)
    if (ERR.get == types.error.ESUCCESS) {
      persistence.persistence_add_gnd(JNLHEAD, types.group_node.mkgnode(ND0, SQNUM, true, false), ADR0, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      persistence.persistence_add_gnd(JNLHEAD, types.group_node.mkgnode(ND1, SQNUM + 1, false, false), ADR1, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      persistence.persistence_add_gnd(JNLHEAD, types.group_node.mkgnode(ND2, SQNUM + 2, false, true), ADR2, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      persistence.persistence_flush_gnd(JNLHEAD, ERR)
    }
    if (ERR.get == types.error.ESUCCESS)
      SQNUM = SQNUM + 3
    else
      JNLHEADVALID = false
  }

  override def journal_add4(ND0: node, ND1: node, ND2: node, ND3: node, ADR0: Ref[address], ADR1: Ref[address], ADR2: Ref[address], ADR3: Ref[address], ERR: Ref[error]): Unit = {
    val SIZE3: Int = flashsize(ND3)
    val SIZE2: Int = flashsize(ND2)
    val SIZE1: Int = flashsize(ND1)
    val SIZE0: Int = flashsize(ND0)
    gjournal_allocate(SIZE0 + (SIZE1 + (SIZE2 + SIZE3)), ERR)
    if (ERR.get == types.error.ESUCCESS) {
      persistence.persistence_add_gnd(JNLHEAD, types.group_node.mkgnode(ND0, SQNUM, true, false), ADR0, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      persistence.persistence_add_gnd(JNLHEAD, types.group_node.mkgnode(ND1, SQNUM + 1, false, false), ADR1, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      persistence.persistence_add_gnd(JNLHEAD, types.group_node.mkgnode(ND2, SQNUM + 2, false, false), ADR2, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      persistence.persistence_add_gnd(JNLHEAD, types.group_node.mkgnode(ND3, SQNUM + 3, false, true), ADR3, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      persistence.persistence_flush_gnd(JNLHEAD, ERR)
    }
    if (ERR.get == types.error.ESUCCESS)
      SQNUM = SQNUM + 4
    else
      JNLHEADVALID = false
  }

  override def journal_gc(ERR: Ref[error]): Unit = {
    val LNUM = new Ref[Int](0)
    persistence.persistence_get_gc_block(LNUM, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val N = new Ref[Int](0)
      persistence.persistence_get_gblock_refsize(LNUM.get, N)
      if (N.get != 0) {
        val GNDLIST1: group_node_list = new group_node_list()
        val KEYLIST: key_list = new key_list()
        val ADRLIST1: address_list = new address_list()
        if (ERR.get == types.error.ESUCCESS) {
          ubifs_gc_copy(LNUM.get, ADRLIST1, GNDLIST1, KEYLIST, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          ubifs_gc_update_index(ADRLIST1, GNDLIST1, KEYLIST, ERR)
        }
      }
    }
    if (ERR.get == types.error.ESUCCESS) {
      persistence.persistence_deallocate_gnd(LNUM.get, ERR)
    }
  }

  override def journal_get(ADR: address, ND: Ref[node], ERR: Ref[error]): Unit = {
    val GND = new Ref[group_node](group_node.uninit)
    persistence.persistence_read_gnd(ADR, GND, ERR)
    if (ERR.get == types.error.ESUCCESS)
      ND := GND.get.nd.deepCopy

  }

  override def orphan_insert(KEY: key): Unit = {
    RO += KEY
  }

  override def orphan_remove(KEY: key): Unit = {
    RO -= KEY
  }

  override def orphans_contains(KEY: key, EXISTS: Ref[Boolean]): Unit = {
    EXISTS := RO.contains(KEY)
  }

  def remove_addresses(AX: address_list): Unit = {
    while (!AX.isEmpty) {
      val ADR: address = AX.head
      val N = new Ref[Int](0)
      persistence.persistence_get_gblock_refsize(ADR.lnum, N)
      persistence.persistence_set_gblock_refsize(ADR.lnum, N.get - ADR.size)
      AX.removeHead
    }
  }

  def ubifs_gc_copy(LNUM: Int, ADRLIST1: address_list, GNDLIST1: group_node_list, KEYLIST: key_list, ERR: Ref[error]): Unit = {
    val GNDLIST: group_node_list = new group_node_list()
    val ADRLIST0: address_list = new address_list()
    persistence.persistence_read_gblock_nodes(LNUM, ADRLIST0, GNDLIST, ERR)
    while (!ADRLIST0.isEmpty && ERR.get == types.error.ESUCCESS) {
      val ND0 = new Ref[node](node.uninit)
      val ADR0 = new Ref[address](address.uninit)
      val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
      val GND: group_node = GNDLIST.head.deepCopy
      val KEY: key = GND.nd.key
      val DIRTY = new Ref[Boolean](false)
      btree_traverse_root(types.modification.lookup(KEY), null, DIRTY, EXISTS, ADR0, ND0, ERR)
      val ADR: address = ADRLIST0.head
      if (ERR.get == types.error.ESUCCESS && (EXISTS.get && ADR0.get == ADR)) {
        gjournal_allocate(ADR.size, ERR)
        val ADR1 = new Ref[address](address.uninit)
        if (ERR.get == types.error.ESUCCESS) {
          persistence.persistence_add_gnd(JNLHEAD, types.group_node.mkgnode(GND.nd, SQNUM, true, true), ADR1, ERR)
          if (ERR.get != types.error.ESUCCESS)
            JNLHEADVALID = false

        }
        if (ERR.get == types.error.ESUCCESS) {
          KEYLIST += KEY
          ADRLIST1 += ADR1.get
          GNDLIST1 += GND
        }
      }
      if (ERR.get == types.error.ESUCCESS) {
        ADRLIST0.removeHead
        GNDLIST.removeHead
      }
    }
    if (JNLHEADVALID && ERR.get == types.error.ESUCCESS) {
      persistence.persistence_flush_gnd(JNLHEAD, ERR)
    }
    if (ERR.get != types.error.ESUCCESS)
      JNLHEADVALID = false

  }

  def ubifs_gc_update_index(ADRLIST1: address_list, GNDLIST1: group_node_list, KEYLIST: key_list, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    while (!ADRLIST1.isEmpty && ERR.get == types.error.ESUCCESS) {
      index_store(KEYLIST.head, ADRLIST1.head, GNDLIST1.head.nd, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        KEYLIST.removeHead
        ADRLIST1.removeHead
        GNDLIST1.removeHead
      }
    }
  }

  def ubifs_remove_nonend_blocks(LNUM: Int, LOG: address_list, ERR: Ref[error]): Unit = {
    val GNDLIST: group_node_list = new group_node_list()
    persistence.persistence_read_gblock_nodes(LNUM, LOG, GNDLIST, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      var DONE: Boolean = false
      while (DONE != true && !GNDLIST.isEmpty) {
        if (GNDLIST.last.end)
          DONE = true
        else {
          LOG.removeLast
          GNDLIST.removeLast
        }
      }
    }
  }

  def ubifs_restore_log(LOG: address_list, ERR: Ref[error]): Unit = {
    LOG.clear
    ERR := types.error.ESUCCESS
    val NL0: nat_list = new nat_list()
    persistence.persistence_read_log(NL0)
    while (!NL0.isEmpty && ERR.get == types.error.ESUCCESS) {
      ubifs_remove_nonend_blocks(NL0.head, LOG, ERR)
      NL0.removeHead
    }
  }

}

object UBIFSJournal {
  def apply(JNLHEADVALID: Boolean, ADRT: address, RO: key_set, MAXINO: Int, SQNUM: Int, JNLHEAD: Int, RT: znode, persistence: AbstractPersistence)(implicit _algebraic_implicit: algebraic.Algebraic) = {
    new UBIFSJournal(JNLHEADVALID, ADRT, RO, MAXINO, SQNUM, JNLHEAD, RT, persistence)
  }
}
