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

class Gjournal(var DOSYNC : Boolean, var JMAXINO : Int, val JRO : nat_set, var JVALID : Boolean, var SQNUM : Int, val index : IndexInterface)(implicit _algebraic_implicit: algebraic.Algebraic, _procedures_implicit: proc.Procedures) extends AubifsCoreInterface {
  import _algebraic_implicit._
  import _procedures_implicit._

  override def check_commit(ERR: Ref[error]): Unit = {
    val COMMIT_ = Ref[Boolean](false)
    index.requires_commit(COMMIT_)
    if (COMMIT_.get) {
      commit(ERR)
    } else {
      ERR := types.error.ESUCCESS
    }
  }

  override def commit(ERR: Ref[error]): Unit = {
    index.sync(ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val JRO0: nat_set = JRO.deepCopy
      index.commit(JMAXINO, JRO0, ERR)
    }
  }

  override def format(VOLSIZE: Int, SIZE: Int, DOSYNC0: Boolean, ERR: Ref[error]): Unit = {
    JMAXINO = 1
    SQNUM = 0
    JVALID = true
    DOSYNC = DOSYNC0
    index.format(VOLSIZE, SIZE, JMAXINO, ERR)
    if (ERR.get != types.error.ESUCCESS) {
      debug("gjournal: persistence format failed")
    }
    JRO.clear
  }

  def gc_copy(NDLIST: node_list, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    while (ERR.get == types.error.ESUCCESS && ! NDLIST.isEmpty) {
      val SIZE = Ref[Int](0)
      val EMPTY_ = Ref[Boolean](helpers.scala.Boolean.uninit)
      index.is_log_empty(EMPTY_)
      if (EMPTY_.get != true) {
        index.get_block_free_size(SIZE)
        if (SIZE.get < flashsize(NDLIST.head)) {
          SIZE := 0
        }
      }
      val NDLIST0: node_list = new node_list()
      val N = Ref[Int](0)
      index.get_leb_size(N)
      gjournal_split_nodes(N.get - SIZE.get, NDLIST, NDLIST0)
      if (NDLIST0.isEmpty) {
        ERR := types.error.EINVAL
      } else {
        val ADRLIST: address_list = new address_list()
        journal_addn(NDLIST0, ADRLIST, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          gc_update_index(ADRLIST, NDLIST0)
        }
      }
    }
  }

  def gc_referenced_nodes(LNUM: Int, ADRLIST: address_list, GNDLIST: group_node_list, NDLIST: node_list, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    NDLIST.clear
    while (ERR.get == types.error.ESUCCESS && ! ADRLIST.isEmpty) {
      val ADR0 = Ref[address](types.address.uninit)
      val EXISTS = Ref[Boolean](helpers.scala.Boolean.uninit)
      val ADR: address = ADRLIST.head
      val GND: group_node = GNDLIST.head.deepCopy
      val KEY: key = GND.nd.key
      index.index_lookup(KEY, EXISTS, ADR0, ERR)
      if (ERR.get == types.error.ESUCCESS && (EXISTS.get && ADR0.get == ADR)) {
        NDLIST += GND.nd
      }
      if (ERR.get == types.error.ESUCCESS) {
        ADRLIST.removeHead
        GNDLIST.removeHead
      }
    }
  }

  def gc_update_index(ADRLIST: address_list, NDLIST: node_list): Unit = {
    while (! ADRLIST.isEmpty) {
      index_store(NDLIST.head.key, ADRLIST.head)
      ADRLIST.removeHead
      NDLIST.removeHead
    }
  }

  override def index_contains(KEY: key, EXISTS: Ref[Boolean], ERR: Ref[error]): Unit = {
    index.index_contains(KEY, EXISTS, ERR)
  }

  override def index_entries(KEY: key, NAMES: stringset, ERR: Ref[error]): Unit = {
    index.index_entries(KEY, NAMES, ERR)
  }

  override def index_lookup(KEY: key, EXISTS: Ref[Boolean], ND: Ref[node], ERR: Ref[error]): Unit = {
    val ADR = Ref[address](types.address.uninit)
    index.index_lookup(KEY, EXISTS, ADR, ERR)
    if (ERR.get == types.error.ESUCCESS && EXISTS.get) {
      val GND = Ref[group_node](types.group_node.uninit)
      index.read_gnd(ADR.get, GND, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        ND := GND.get.nd.deepCopy
      }
    }
  }

  override def index_newino(KEY: Ref[key]): Unit = {
    KEY := types.key.inodekey(JMAXINO)
    JMAXINO = JMAXINO + 1
  }

  override def index_remove(KEY: key): Unit = {
    val OLDADR = Ref[address](types.address.uninit)
    val EXISTS = Ref[Boolean](helpers.scala.Boolean.uninit)
    index.index_remove(KEY, OLDADR, EXISTS)
    if (EXISTS.get) {
      val N = Ref[Int](0)
      index.get_gblock_refsize(OLDADR.get.lnum, N)
      index.set_gblock_refsize(OLDADR.get.lnum, N.get - OLDADR.get.size)
    }
  }

  override def index_store(KEY: key, ADR: address): Unit = {
    val OLDADR = Ref[address](types.address.uninit)
    val EXISTS = Ref[Boolean](helpers.scala.Boolean.uninit)
    index.index_store(KEY, ADR, OLDADR, EXISTS)
    val N = Ref[Int](0)
    if (EXISTS.get) {
      index.get_gblock_refsize(OLDADR.get.lnum, N)
      index.set_gblock_refsize(OLDADR.get.lnum, N.get - OLDADR.get.size)
    }
    index.get_gblock_refsize(ADR.lnum, N)
    index.set_gblock_refsize(ADR.lnum, N.get + ADR.size)
    JMAXINO = max(KEY.ino + 1, JMAXINO)
  }

  override def index_truncate(KEY: key, N: Int): Unit = {
    val AS: address_set = new address_set()
    index.index_truncate(KEY, N, AS)
    remove_addresses(AS)
  }

  override def journal_add1(ND1: node, ADR1: Ref[address], ERR: Ref[error]): Unit = {
    val ADRLIST: address_list = new address_list()
    val NDLIST: node_list = new node_list()
    ND1 +=: NDLIST
    journal_addn(NDLIST, ADRLIST, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      ADR1 := ADRLIST(0)
    }
  }

  override def journal_add2(ND1: node, ND2: node, ADR1: Ref[address], ADR2: Ref[address], ERR: Ref[error]): Unit = {
    val ADRLIST: address_list = new address_list()
    val NDLIST: node_list = new node_list()
    ND2 +=: NDLIST
    ND1 +=: NDLIST
    journal_addn(NDLIST, ADRLIST, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      ADR1 := ADRLIST(0)
      ADR2 := ADRLIST(1)
    }
  }

  override def journal_add3(ND1: node, ND2: node, ND3: node, ADR1: Ref[address], ADR2: Ref[address], ADR3: Ref[address], ERR: Ref[error]): Unit = {
    val ADRLIST: address_list = new address_list()
    val NDLIST: node_list = new node_list()
    ND3 +=: NDLIST
    ND2 +=: NDLIST
    ND1 +=: NDLIST
    journal_addn(NDLIST, ADRLIST, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      ADR1 := ADRLIST(0)
      ADR2 := ADRLIST(1)
      ADR3 := ADRLIST(2)
    }
  }

  override def journal_add4(ND1: node, ND2: node, ND3: node, ND4: node, ADR1: Ref[address], ADR2: Ref[address], ADR3: Ref[address], ADR4: Ref[address], ERR: Ref[error]): Unit = {
    val ADRLIST: address_list = new address_list()
    val NDLIST: node_list = new node_list()
    ND4 +=: NDLIST
    ND3 +=: NDLIST
    ND2 +=: NDLIST
    ND1 +=: NDLIST
    journal_addn(NDLIST, ADRLIST, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      ADR1 := ADRLIST(0)
      ADR2 := ADRLIST(1)
      ADR3 := ADRLIST(2)
      ADR4 := ADRLIST(3)
    }
  }

  override def journal_add5(ND1: node, ND2: node, ND3: node, ND4: node, ND5: node, ADR1: Ref[address], ADR2: Ref[address], ADR3: Ref[address], ADR4: Ref[address], ADR5: Ref[address], ERR: Ref[error]): Unit = {
    val ADRLIST: address_list = new address_list()
    val NDLIST: node_list = new node_list()
    ND5 +=: NDLIST
    ND4 +=: NDLIST
    ND3 +=: NDLIST
    ND2 +=: NDLIST
    ND1 +=: NDLIST
    journal_addn(NDLIST, ADRLIST, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      ADR1 := ADRLIST(0)
      ADR2 := ADRLIST(1)
      ADR3 := ADRLIST(2)
      ADR4 := ADRLIST(3)
      ADR5 := ADRLIST(4)
    }
  }

  def journal_addn(NDLIST: node_list, ADRLIST: address_list, ERR: Ref[error]): Unit = {
    val GNDLIST: group_node_list = new group_node_list()
    val SIZE = Ref[Int](0)
    journal_transaction(NDLIST, GNDLIST, SIZE)
    journal_allocate(SIZE.get, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      index.add_gnds(GNDLIST, ADRLIST, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        SQNUM = SQNUM + NDLIST.length
        if (DOSYNC) {
          val ERR0 = Ref[error](types.error.uninit)
          index.sync(ERR0)
        }
      } else {
        JVALID = false
      }
    }
  }

  def journal_allocate(SIZE: Int, ERR: Ref[error]): Unit = {
    val N = Ref[Int](0)
    index.get_leb_size(N)
    if (SIZE > N.get) {
      ERR := types.error.ENOSPC
    } else {
      val EMPTY_ = Ref[Boolean](helpers.scala.Boolean.uninit)
      index.is_log_empty(EMPTY_)
      val N = Ref[Int](0)
      if (EMPTY_.get != true && JVALID) {
        index.get_block_free_size(N)
      }
      if (EMPTY_.get != true && (JVALID && SIZE <= N.get)) {
        ERR := types.error.ESUCCESS
      } else {
        index.sync(ERR)
        if (ERR.get == types.error.ESUCCESS) {
          index.allocate_gnd(ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          JVALID = true
        }
      }
    }
  }

  override def journal_gc(): Unit = {
    val ERR = Ref[error](types.error.uninit)
    val LNUM = Ref[Int](0)
    index.get_gc_block(LNUM, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      debug("gjournal: garbage collecting LEB " + toStr(LNUM.get))
      val N = Ref[Int](0)
      index.get_gblock_refsize(LNUM.get, N)
      if (N.get != 0) {
        val ADRLIST: address_list = new address_list()
        val GNDLIST: group_node_list = new group_node_list()
        index.read_gblock_nodes(LNUM.get, ADRLIST, GNDLIST, ERR)
        val NDLIST: node_list = new node_list()
        if (ERR.get != types.error.ESUCCESS) {
          debug("gjournal: failed to read LEB " + (toStr(LNUM.get) + " during GC"))
        } else {
          gc_referenced_nodes(LNUM.get, ADRLIST, GNDLIST, NDLIST, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          gc_copy(NDLIST, ERR)
          if (ERR.get != types.error.ESUCCESS) {
            debug("gjournal: failed to copy nodes of LEB " + (toStr(LNUM.get) + " to journal head during GC"))
          }
        }
      }
    }
    if (ERR.get == types.error.ESUCCESS) {
      index.sync(ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      index.deallocate_gnd(LNUM.get, ERR)
    }
  }

  override def journal_get(ADR: address, ND: Ref[node], ERR: Ref[error]): Unit = {
    val GND = Ref[group_node](types.group_node.uninit)
    index.read_gnd(ADR, GND, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      ND := GND.get.nd.deepCopy
    }
  }

  override def journal_sync(ERR: Ref[error]): Unit = {
    index.sync(ERR)
  }

  def journal_transaction(NDLIST1: node_list, GNDLIST: group_node_list, SIZE: Ref[Int]): Unit = {
    val NDLIST: node_list = NDLIST1.deepCopy
    SIZE := 0
    GNDLIST.clear
    var FIRST_ : Boolean = true
    while (! NDLIST.isEmpty) {
      val LAST_ : Boolean = NDLIST.length == 1
      val GND: group_node = types.group_node.mkgnode(NDLIST.head, SQNUM + GNDLIST.length, FIRST_, LAST_).deepCopy
      GNDLIST += GND
      SIZE := SIZE.get + flashsize(GND)
      NDLIST.removeHead
      FIRST_ = false
    }
  }

  override def orphans_contains(KEY: key, EXISTS: Ref[Boolean]): Unit = {
    EXISTS := JRO.contains(KEY.ino)
  }

  override def orphans_insert(KEY: key): Unit = {
    JRO += KEY.ino
    JMAXINO = max(KEY.ino + 1, JMAXINO)
  }

  override def orphans_remove(KEY: key): Unit = {
    JRO -= KEY.ino
  }

  override def recover(DOSYNC0: Boolean, LOG: address_list, KS: key_set, ERR: Ref[error]): Unit = {
    SQNUM = 0
    JVALID = false
    DOSYNC = DOSYNC0
    val JNL: nat_list = new nat_list()
    
    {
      val nat_variable0: Ref[Int] = Ref[Int](JMAXINO)
      index.recover(nat_variable0, JNL, JRO, ERR)
      JMAXINO = nat_variable0.get
    }
    if (ERR.get != types.error.ESUCCESS) {
      debug("gjournal: persistence recover failed")
    } else {
      restore_log(JNL, LOG, ERR)
      KS := keys(JRO).deepCopy
    }
  }

  def remove_addresses(AS: address_set): Unit = {
    while (! AS.isEmpty) {
      val N = Ref[Int](0)
      val ADR: address = AS.head
      index.get_gblock_refsize(ADR.lnum, N)
      index.set_gblock_refsize(ADR.lnum, N.get - ADR.size)
      AS -= ADR
    }
  }

  def remove_nonend_blocks(LNUM: Int, LOG: address_list, ERR: Ref[error]): Unit = {
    val GNDLIST: group_node_list = new group_node_list()
    index.read_gblock_nodes(LNUM, LOG, GNDLIST, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      var DONE: Boolean = false
      while (DONE != true && ! GNDLIST.isEmpty) {
        if (GNDLIST.last.end) {
          DONE = true
        } else {
          LOG.removeLast
          GNDLIST.removeLast
        }
      }
    }
  }

  def restore_log(JNL1: nat_list, LOG: address_list, ERR: Ref[error]): Unit = {
    val JNL: nat_list = JNL1.deepCopy
    LOG.clear
    ERR := types.error.ESUCCESS
    while (! JNL.isEmpty && ERR.get == types.error.ESUCCESS) {
      remove_nonend_blocks(JNL.head, LOG, ERR)
      JNL.removeHead
    }
  }

}
