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

class gjournal_asm(var SQNUM : Int, var JNLHEADVALID : Boolean, val JRO : nat_set, var JMAXINO : Int, var SYNC : Boolean, var JNLHEAD : Int, val indexpluspersistence : indexpluspersistence_interface)(implicit _algebraic_implicit: algebraic.Algebraic, _procedures_implicit: proc.Procedures) extends aubifs_internal_asm_interface {
  import _algebraic_implicit._
  import _procedures_implicit._

  override def aubifs_commit(ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    if (JNLHEADVALID && SYNC != true) {
      indexpluspersistence.indexpluspersistence_flush_gnd(JNLHEAD, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      val JRO0: nat_set = JRO.deepCopy
      indexpluspersistence.indexpluspersistence_commit(JMAXINO, JRO0, ERR)
    }
    JNLHEADVALID = false
  }

  override def aubifs_internal_check_commit(ERR: Ref[error]): Unit = {
    val COMMIT_ = new Ref[Boolean](false)
    indexpluspersistence.indexpluspersistence_requires_commit(COMMIT_)
    if (COMMIT_.get) {
      aubifs_commit(ERR)
    } else
      ERR := types.error.ESUCCESS
  }

  override def aubifs_readflash(LOG: address_list, KS: key_set, ERR: Ref[error]): Unit = {
    JNLHEADVALID = false
    SYNC = true
    SQNUM = 0
    val JNL: nat_list = new nat_list()
    val ino: Ref[Int] = new Ref[Int](JMAXINO)
    indexpluspersistence.indexpluspersistence_recover(ino, JNL, JRO, ERR)
    JMAXINO = ino.get
    if (ERR.get != types.error.ESUCCESS) {
      debug("gjournal: persistence recover failed")
    } else {
      gjournal_restore_log(JNL, LOG, ERR)
      KS := keys(JRO).deepCopy
    }
  }

  def gjournal_addn(NDLIST: node_list, ADRLIST: address_list, ERR: Ref[error]): Unit = {
    val GNDLIST: group_node_list = new group_node_list()
    val SIZE = new Ref[Int](0)
    gjournal_transaction(NDLIST, GNDLIST, SIZE)
    gjournal_allocate(SIZE.get, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      indexpluspersistence.indexpluspersistence_add_gnds(JNLHEAD, GNDLIST, ADRLIST, ERR)
    }
    if (ERR.get == types.error.ESUCCESS && SYNC) {
      indexpluspersistence.indexpluspersistence_flush_gnd(JNLHEAD, ERR)
    }
    if (ERR.get == types.error.ESUCCESS)
      SQNUM = SQNUM + GNDLIST.length
    else
      JNLHEADVALID = false
  }

  def gjournal_allocate(SIZE: Int, ERR: Ref[error]): Unit = {
    if (SIZE > LEB_SIZE)
      ERR := types.error.ENOSPC
    else {
      val N = new Ref[Int](0)
      if (JNLHEADVALID) {
        indexpluspersistence.indexpluspersistence_get_gblock_size(JNLHEAD, N)
      }
      if (JNLHEADVALID && N.get + SIZE <= LEB_SIZE)
        ERR := types.error.ESUCCESS
      else {
        ERR := types.error.ESUCCESS
        if (JNLHEADVALID && SYNC != true) {
          indexpluspersistence.indexpluspersistence_flush_gnd(JNLHEAD, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          indexpluspersistence.indexpluspersistence_allocate_gnd(N, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          JNLHEADVALID = true
          JNLHEAD = N.get
        } else
          JNLHEADVALID = false
      }
    }
  }

  def gjournal_gc_copy(NDLIST: node_list, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    while (ERR.get == types.error.ESUCCESS && ! NDLIST.isEmpty) {
      val SIZE = new Ref[Int](0)
      if (JNLHEADVALID) {
        indexpluspersistence.indexpluspersistence_get_gblock_size(JNLHEAD, SIZE)
        if (SIZE.get + flashsize(NDLIST.head) > LEB_SIZE)
          SIZE := 0
        
      }
      val NDLIST0: node_list = new node_list()
      gjournal_split_nodes(LEB_SIZE - SIZE.get, NDLIST, NDLIST0)
      if (NDLIST0.isEmpty)
        ERR := types.error.EINVAL
      else {
        val ADRLIST: address_list = new address_list()
        gjournal_addn(NDLIST0, ADRLIST, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          gjournal_gc_update_index(ADRLIST, NDLIST0)
        }
      }
    }
  }

  def gjournal_gc_referenced_nodes(LNUM: Int, ADRLIST: address_list, GNDLIST: group_node_list, NDLIST: node_list, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    NDLIST.clear
    while (ERR.get == types.error.ESUCCESS && ! ADRLIST.isEmpty) {
      val ND0 = new Ref[node](types.node.uninit)
      val ADR0 = new Ref[address](types.address.uninit)
      val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
      val GND: group_node = GNDLIST.head.deepCopy
      val KEY: key = GND.nd.key
      indexpluspersistence.indexpluspersistence_index_lookup(KEY, EXISTS, ADR0, ND0, ERR)
      val ADR: address = ADRLIST.head
      if (ERR.get == types.error.ESUCCESS && (EXISTS.get && ADR0.get == ADR))
        NDLIST += GND.nd
      
      if (ERR.get == types.error.ESUCCESS) {
        ADRLIST.removeHead
        GNDLIST.removeHead
      }
    }
  }

  def gjournal_gc_update_index(ADRLIST: address_list, NDLIST: node_list): Unit = {
    while (! ADRLIST.isEmpty) {
      val ND: node = NDLIST.head.deepCopy
      val ERR = new Ref[error](types.error.uninit)
      index_store(ND.key, ADRLIST.head, ND, ERR)
      ADRLIST.removeHead
      NDLIST.removeHead
    }
  }

  def gjournal_remove_addresses(AS: address_set): Unit = {
    while (! AS.isEmpty) {
      val N = new Ref[Int](0)
      val ADR: address = AS.head
      indexpluspersistence.indexpluspersistence_get_gblock_refsize(ADR.lnum, N)
      indexpluspersistence.indexpluspersistence_set_gblock_refsize(ADR.lnum, N.get - ADR.size)
      AS -= ADR
    }
  }

  def gjournal_remove_nonend_blocks(LNUM: Int, LOG: address_list, ERR: Ref[error]): Unit = {
    val GNDLIST: group_node_list = new group_node_list()
    indexpluspersistence.indexpluspersistence_read_gblock_nodes(LNUM, LOG, GNDLIST, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      var DONE: Boolean = false
      while (DONE != true && ! GNDLIST.isEmpty) {
        if (GNDLIST.last.end)
          DONE = true
        else {
          LOG.removeLast
          GNDLIST.removeLast
        }
      }
    }
  }

  def gjournal_restore_log(NL: nat_list, LOG: address_list, ERR: Ref[error]): Unit = {
    val JNL: nat_list = NL.deepCopy
    LOG.clear
    ERR := types.error.ESUCCESS
    while (! JNL.isEmpty && ERR.get == types.error.ESUCCESS) {
      gjournal_remove_nonend_blocks(JNL.head, LOG, ERR)
      JNL.removeHead
    }
  }

  def gjournal_split_nodes(N: Int, NDLIST: node_list, NDLIST0: node_list): Unit = {
    var SIZE: Int = N
    NDLIST0.clear
    while (! NDLIST.isEmpty && flashsize(NDLIST.head) <= SIZE) {
      val ND: node = NDLIST.head.deepCopy
      NDLIST0 += ND
      NDLIST.removeHead
      SIZE = SIZE - flashsize(ND)
    }
  }

  def gjournal_transaction(NDLIST0: node_list, GNDLIST: group_node_list, SIZE: Ref[Int]): Unit = {
    val NDLIST: node_list = NDLIST0.deepCopy
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

  override def index_checkdata(KEY: key, N: Int, ERR: Ref[error]): Unit = {
    indexpluspersistence.indexpluspersistence_index_checkdata(KEY, N, ERR)
  }

  override def index_checkkey(KEY: key, EXISTS: Ref[Boolean], ERR: Ref[error]): Unit = {
    indexpluspersistence.indexpluspersistence_index_checkkey(KEY, EXISTS, ERR)
  }

  override def index_contains(KEY: key, EXISTS: Ref[Boolean], ERR: Ref[error]): Unit = {
    indexpluspersistence.indexpluspersistence_index_contains(KEY, EXISTS, ERR)
  }

  override def index_entries(KEY: key, NAMES: stringset, ERR: Ref[error]): Unit = {
    indexpluspersistence.indexpluspersistence_index_entries(KEY, NAMES, ERR)
  }

  override def index_lookup(KEY: key, EXISTS: Ref[Boolean], ND: Ref[node], ERR: Ref[error]): Unit = {
    val ADR = new Ref[address](types.address.uninit)
    indexpluspersistence.indexpluspersistence_index_lookup(KEY, EXISTS, ADR, ND, ERR)
  }

  override def index_newino(KEY: Ref[key]): Unit = {
    KEY := types.key.inodekey(JMAXINO)
    JMAXINO = JMAXINO + 1
  }

  override def index_remove(KEY: key): Unit = {
    val OLDADR = new Ref[address](types.address.uninit)
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    indexpluspersistence.indexpluspersistence_index_remove(KEY, OLDADR, EXISTS)
    if (EXISTS.get) {
      val N = new Ref[Int](0)
      indexpluspersistence.indexpluspersistence_get_gblock_refsize(OLDADR.get.lnum, N)
      indexpluspersistence.indexpluspersistence_set_gblock_refsize(OLDADR.get.lnum, N.get - OLDADR.get.size)
    }
  }

  override def index_store(KEY: key, ADR: address, ND: node, ERR: Ref[error]): Unit = {
    val OLDADR = new Ref[address](types.address.uninit)
    val EXISTS = new Ref[Boolean](helpers.scala.Boolean.uninit)
    indexpluspersistence.indexpluspersistence_index_store(KEY, ADR, ND, OLDADR, EXISTS, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val N = new Ref[Int](0)
      if (EXISTS.get) {
        indexpluspersistence.indexpluspersistence_get_gblock_refsize(OLDADR.get.lnum, N)
        indexpluspersistence.indexpluspersistence_set_gblock_refsize(OLDADR.get.lnum, N.get - OLDADR.get.size)
      }
      indexpluspersistence.indexpluspersistence_get_gblock_refsize(ADR.lnum, N)
      indexpluspersistence.indexpluspersistence_set_gblock_refsize(ADR.lnum, N.get + ADR.size)
      JMAXINO = max(KEY.ino + 1, JMAXINO)
    }
  }

  override def index_truncate(KEY: key, N: Int, ERR: Ref[error]): Unit = {
    val AS: address_set = new address_set()
    indexpluspersistence.indexpluspersistence_index_truncate(KEY, N, AS, ERR)
    gjournal_remove_addresses(AS)
  }

  override def internal_format(VOLSIZE: Int, ERR: Ref[error]): Unit = {
    JNLHEADVALID = false
    JMAXINO = ROOT_INO + 1
    SQNUM = 0
    SYNC = true
    indexpluspersistence.indexpluspersistence_format(VOLSIZE, JMAXINO, ERR)
    if (ERR.get != types.error.ESUCCESS) {
      debug("gjournal: persistence format failed")
    }
    JRO.clear
  }

  override def journal_add1(ND0: node, ADR0: Ref[address], ERR: Ref[error]): Unit = {
    val ADRLIST: address_list = new address_list()
    val NDLIST: node_list = new node_list()
    ND0 +=: NDLIST
    gjournal_addn(NDLIST, ADRLIST, ERR)
    if (ERR.get == types.error.ESUCCESS)
      ADR0 := at(ADRLIST, 0)
    
  }

  override def journal_add2(ND0: node, ND1: node, ADR0: Ref[address], ADR1: Ref[address], ERR: Ref[error]): Unit = {
    val ADRLIST: address_list = new address_list()
    val NDLIST: node_list = new node_list()
    ND1 +=: NDLIST
    ND0 +=: NDLIST
    gjournal_addn(NDLIST, ADRLIST, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      ADR0 := at(ADRLIST, 0)
      ADR1 := at(ADRLIST, 1)
    }
  }

  override def journal_add3(ND0: node, ND1: node, ND2: node, ADR0: Ref[address], ADR1: Ref[address], ADR2: Ref[address], ERR: Ref[error]): Unit = {
    val ADRLIST: address_list = new address_list()
    val NDLIST: node_list = new node_list()
    ND2 +=: NDLIST
    ND1 +=: NDLIST
    ND0 +=: NDLIST
    gjournal_addn(NDLIST, ADRLIST, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      ADR0 := at(ADRLIST, 0)
      ADR1 := at(ADRLIST, 1)
      ADR2 := at(ADRLIST, 2)
    }
  }

  override def journal_add4(ND0: node, ND1: node, ND2: node, ND3: node, ADR0: Ref[address], ADR1: Ref[address], ADR2: Ref[address], ADR3: Ref[address], ERR: Ref[error]): Unit = {
    val ADRLIST: address_list = new address_list()
    val NDLIST: node_list = new node_list()
    ND3 +=: NDLIST
    ND2 +=: NDLIST
    ND1 +=: NDLIST
    ND0 +=: NDLIST
    gjournal_addn(NDLIST, ADRLIST, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      ADR0 := at(ADRLIST, 0)
      ADR1 := at(ADRLIST, 1)
      ADR2 := at(ADRLIST, 2)
      ADR3 := at(ADRLIST, 3)
    }
  }

  override def journal_gc(): Unit = {
    val ERR = new Ref[error](types.error.uninit)
    val LNUM = new Ref[Int](0)
    indexpluspersistence.indexpluspersistence_get_gc_block(LNUM, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      debug("gjournal: garbage collecting LEB " + toStr(LNUM.get))
      val N = new Ref[Int](0)
      indexpluspersistence.indexpluspersistence_get_gblock_refsize(LNUM.get, N)
      if (N.get != 0) {
        val GNDLIST: group_node_list = new group_node_list()
        val ADRLIST: address_list = new address_list()
        indexpluspersistence.indexpluspersistence_read_gblock_nodes(LNUM.get, ADRLIST, GNDLIST, ERR)
        val NDLIST: node_list = new node_list()
        if (ERR.get != types.error.ESUCCESS) {
          debug("gjournal: failed to read LEB " + (toStr(LNUM.get) + " during GC"))
        } else {
          gjournal_gc_referenced_nodes(LNUM.get, ADRLIST, GNDLIST, NDLIST, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          gjournal_gc_copy(NDLIST, ERR)
          if (ERR.get != types.error.ESUCCESS) {
            debug("gjournal: failed to copy nodes of LEB " + (toStr(LNUM.get) + " to journal head during GC"))
          }
        }
      }
    }
    if (ERR.get == types.error.ESUCCESS) {
      indexpluspersistence.indexpluspersistence_deallocate_gnd(LNUM.get, ERR)
    }
  }

  override def journal_get(ADR: address, ND: Ref[node], ERR: Ref[error]): Unit = {
    val GND = new Ref[group_node](types.group_node.uninit)
    indexpluspersistence.indexpluspersistence_read_gnd(ADR, GND, ERR)
    if (ERR.get == types.error.ESUCCESS)
      ND := GND.get.nd.deepCopy
    
  }

  override def orphan_insert(KEY: key): Unit = {
    JRO += KEY.ino
    JMAXINO = max(KEY.ino + 1, JMAXINO)
  }

  override def orphan_remove(KEY: key): Unit = {
    JRO -= KEY.ino
  }

  override def orphans_contains(KEY: key, EXISTS: Ref[Boolean]): Unit = {
    EXISTS := JRO.contains(KEY.ino)
  }

}

