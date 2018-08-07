// Flashix: a verified file system for flash memory
// (c) 2015-2018 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import encoding.node_header._
import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._
import proc._
import types._
import types.error.error
import types.lpropflags.lpropflags

class Persistence(val FREELIST : nat_list, val GCHEAP : binheap, val Gcarray : gc_array, var LEBSIZE : Int, val LOG : nat_list, val LPT : lp_array, val awbuf : AwbufInterface)(implicit _algebraic_implicit: algebraic.Algebraic, _procedures_implicit: proc.Procedures) extends ApersistenceInterface {
  import _algebraic_implicit._
  import _procedures_implicit._

  override def add_gnds(NODELIST: group_node_list, ADRLIST: address_list, ERR: Ref[error]): Unit = {
    val AROFS0 = Ref[Boolean](helpers.scala.Boolean.uninit)
    awbuf.is_readonly(AROFS0)
    if (AROFS0.get) {
      ERR := types.error.EROFS
    } else     if (NODELIST.isEmpty) {
      ADRLIST.clear
      ERR := types.error.ESUCCESS
    } else {
      val WBUFLEB0 = Ref[bufleb](types.bufleb.uninit)
      awbuf.get_buf(WBUFLEB0)
      val BUF: buffer = new buffer()
      persistence_encode_group_nodes(NODELIST, WBUFLEB0.get.leb, LPT(WBUFLEB0.get.leb).size, BUF, ADRLIST, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (! (LPT(WBUFLEB0.get.leb).size + BUF.length <= LEBSIZE)) {
          debug("persistence: trying to write beyond LEB size")
          awbuf.enter_readonly()
          ERR := types.error.ENOSPC
        } else {
          awbuf.write_buf(BUF.length, BUF, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            LPT(WBUFLEB0.get.leb).size = LPT(WBUFLEB0.get.leb).size + BUF.length
          } else {
            debug("persistence: adding group node to LEB " + (toStr(WBUFLEB0.get.leb) + " failed"))
            LPT(WBUFLEB0.get.leb).size = LEBSIZE
          }
        }
      } else {
        debug("persistence: could not encode group node")
        awbuf.enter_readonly()
      }
    }
  }

  override def add_ind(IND: index_node, ADR: Ref[address], ERR: Ref[error]): Unit = {
    val AROFS0 = Ref[Boolean](helpers.scala.Boolean.uninit)
    awbuf.is_readonly(AROFS0)
    if (AROFS0.get) {
      ERR := types.error.EROFS
    } else {
      val WBUFLEB0 = Ref[bufleb](types.bufleb.uninit)
      awbuf.get_buf(WBUFLEB0)
      val BUF: buffer = new buffer(flashsize(IND)).fill(0.toByte)
      val SIZE = Ref[Int](0)
      encode_index_node(IND, 0, SIZE, BUF, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (! (LPT(WBUFLEB0.get.leb).size + SIZE.get <= LEBSIZE)) {
          debug("persistence: trying to write beyond LEB size")
          awbuf.enter_readonly()
          ERR := types.error.ENOSPC
        } else {
          awbuf.write_buf(SIZE.get, BUF, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            ADR := types.address.at(WBUFLEB0.get.leb, LPT(WBUFLEB0.get.leb).size, SIZE.get)
            LPT(WBUFLEB0.get.leb).size = LPT(WBUFLEB0.get.leb).size + SIZE.get
          } else {
            LPT(WBUFLEB0.get.leb).size = LEBSIZE
          }
        }
      } else {
        debug("persistence: could not encode index node")
        awbuf.enter_readonly()
      }
    }
  }

  override def allocate_gnd(ERR: Ref[error]): Unit = {
    val N = Ref[Int](0)
    allocate_leb(N, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      awbuf.add_log_leb(N.get, 0, ERR)
      if (ERR.get != types.error.ESUCCESS) {
        N.get +=: FREELIST
      }
    }
    if (ERR.get == types.error.ESUCCESS) {
      LPT(N.get).flags = types.lpropflags.LP_GROUP_NODES
      LOG += N.get
    }
  }

  override def allocate_ind(ERR: Ref[error]): Unit = {
    val N = Ref[Int](0)
    allocate_leb(N, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      awbuf.move_buf(N.get, 0, ERR)
      if (ERR.get != types.error.ESUCCESS) {
        N.get +=: FREELIST
      } else {
        LPT(N.get).flags = types.lpropflags.LP_INDEX_NODES
      }
    }
  }

  def allocate_leb(N: Ref[Int], ERR: Ref[error]): Unit = {
    val AROFS0 = Ref[Boolean](helpers.scala.Boolean.uninit)
    awbuf.is_readonly(AROFS0)
    if (AROFS0.get) {
      ERR := types.error.EROFS
    } else     if (FREELIST.isEmpty) {
      ERR := types.error.ENOSPC
    } else {
      N := FREELIST.head
      awbuf.remap(N.get, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        FREELIST.removeHead
        LPT(N.get).size = 0
        LPT(N.get).ref_size = 0
      }
    }
  }

  def cleanup_lpt(): Unit = {
    var N: Int = 0
    while (N < LPT.length) {
      if (LPT(N).ref_size == 0 && LPT(N).flags != types.lpropflags.LP_FREE) {
        if (LPT(N).flags == types.lpropflags.LP_GROUP_NODES) {
          gc_heap_remove(Gcarray(N), GCHEAP, Gcarray)
        }
        LPT(N).flags = types.lpropflags.LP_FREE
        LPT(N).size = 0
        FREELIST += N
        awbuf.unmap(N)
      }
      N = N + 1
    }
  }

  override def commit(WROOTADR0: address, WMAXINO0: Int, ORPHANS0: nat_set, ERR: Ref[error]): Unit = {
    awbuf.destroy_buf(ERR)
    if (ERR.get == types.error.ESUCCESS) {
      awbuf.commit(LPT, WROOTADR0, WMAXINO0, ORPHANS0, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      gc_heap_add_log()
      cleanup_lpt()
    }
  }

  def create_freelist(): Unit = {
    FREELIST.clear
    var N: Int = 0
    while (N < LPT.length) {
      if (LPT(N).flags == types.lpropflags.LP_FREE) {
        FREELIST += N
      }
      N = N + 1
    }
  }

  def create_gcheap(): Unit = {
    gc_heap_empty(GCHEAP)
    var N: Int = 0
    while (N < LPT.length) {
      if (LPT(N).flags == types.lpropflags.LP_GROUP_NODES && ! LOG.contains(N)) {
        val KI: keyindex = types.keyindex.key_index(LPT(N).ref_size, N)
        gc_heap_insert(KI, GCHEAP, Gcarray)
      }
      N = N + 1
    }
  }

  override def deallocate_gnd(N: Int, ERR: Ref[error]): Unit = {
    val AROFS0 = Ref[Boolean](helpers.scala.Boolean.uninit)
    awbuf.is_readonly(AROFS0)
    if (AROFS0.get) {
      ERR := types.error.EROFS
    } else {
      awbuf.unmap(N)
      LPT(N).flags = types.lpropflags.LP_FREE
      LPT(N).ref_size = 0
      LPT(N).size = 0
      FREELIST += N
      gc_heap_remove(Gcarray(N), GCHEAP, Gcarray)
      ERR := types.error.ESUCCESS
    }
  }

  override def format(VOLSIZE: Int, SIZE: Int, WMAXINO0: Int, ERR: Ref[error]): Unit = {
    LPT.allocate(VOLSIZE, types.lprops.uninit)
    Gcarray.allocate(VOLSIZE, 0)
    LPT.fill(types.lprops.mklp(0, 0, types.lpropflags.LP_FREE))
    val ADR: address = types.address.at(0, 0, 0)
    awbuf.format(VOLSIZE, SIZE, LPT, ADR, WMAXINO0, ERR)
    if (ERR.get != types.error.ESUCCESS) {
      debug("persistence: wbuf format failed")
    } else {
      
      {
        val mode: Ref[Int] = Ref[Int](LEBSIZE)
        awbuf.get_leb_size(mode)
        LEBSIZE = mode.get
      }
      LOG.clear
      create_freelist()
      gc_heap_empty(GCHEAP)
    }
  }

  def gc_heap_add_log(): Unit = {
    while (! LOG.isEmpty) {
      val LNUM: Int = LOG.head
      val KI: keyindex = types.keyindex.key_index(LPT(LNUM).ref_size, LNUM)
      gc_heap_insert(KI, GCHEAP, Gcarray)
      LOG.removeHead
    }
  }

  override def get_block_free_size(N: Ref[Int]): Unit = {
    val WBUFLEB0 = Ref[bufleb](types.bufleb.uninit)
    awbuf.get_buf(WBUFLEB0)
    if (WBUFLEB0.get == types.bufleb.nobuffer) {
      N := 0
    } else {
      N := LEBSIZE - LPT(WBUFLEB0.get.leb).size
    }
  }

  override def get_gblock_refsize(LNUM: Int, N: Ref[Int]): Unit = {
    N := LPT(LNUM).ref_size
  }

  override def get_gc_block(N: Ref[Int], ERR: Ref[error]): Unit = {
    val empty_ = Ref[Boolean](helpers.scala.Boolean.uninit)
    gc_heap_is_empty(GCHEAP, empty_)
    if (empty_.get) {
      ERR := types.error.ENOSPC
    } else {
      gc_heap_get_min(GCHEAP, N)
      ERR := types.error.ESUCCESS
    }
  }

  override def get_iblock_refsize(LNUM: Int, N: Ref[Int]): Unit = {
    N := LPT(LNUM).ref_size
  }

  override def get_leb_size(N: Ref[Int]): Unit = {
    N := LEBSIZE
  }

  override def is_log_empty(EMPTY_ : Ref[Boolean]): Unit = {
    EMPTY_ := LOG.isEmpty
  }

  override def is_readonly(ROFS: Ref[Boolean]): Unit = {
    awbuf.is_readonly(ROFS)
  }

  override def read_gblock_nodes(LNUM: Int, ADRLIST: address_list, NODELIST: group_node_list, ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(LEBSIZE).fill(0.toByte)
    awbuf.read_buf(LNUM, 0, LEBSIZE, BUF, ERR)
    var OFFSET: Int = 0
    while (ERR.get == types.error.ESUCCESS && OFFSET < LEBSIZE) {
      if (! (OFFSET + NODE_HEADER_SIZE <= BUF.length)) {
        ERR := types.error.ENOENT
      } else {
        val EMPTY_ = Ref[Boolean](helpers.scala.Boolean.uninit)
        val NDHD = Ref[node_header](types.node_header.uninit)
        decode_header(OFFSET, BUF, NDHD, EMPTY_, ERR)
        if (ERR.get != types.error.ESUCCESS) {
          debug("persistence: read_gblock_nodes could not decode header in LEB " + (toStr(LNUM) + (" at offset " + toStr(OFFSET))))
        } else         if (EMPTY_.get) {
          ERR := types.error.ENOENT
        } else {
          val SIZE: Int = 2 * NODE_HEADER_SIZE + alignUp(NDHD.get.size, 2 * NODE_HEADER_SIZE)
          if (OFFSET + SIZE > LEBSIZE) {
            debug("persistence: read_gblock_nodes ignoring node of size " + (toStr(SIZE) + (" at offset " + (toStr(OFFSET) + (" in LEB " + toStr(LNUM))))))
            ERR := types.error.ENOENT
          } else {
            val EQUALS_ = Ref[Boolean](helpers.scala.Boolean.uninit)
            rangeeq(BUF, OFFSET + (NODE_HEADER_SIZE + alignUp(NDHD.get.size, 2 * NODE_HEADER_SIZE)), validtrailer, 0, NODE_HEADER_SIZE, EQUALS_)
            if (EQUALS_.get != true) {
              ERR := types.error.ENOENT
              debug("persistence: read_gblock_nodes partial node at offset " + (toStr(OFFSET) + (" in LEB " + toStr(LNUM))))
            } else             if (! NDHD.get.ispadding) {
              val GND = Ref[group_node](types.group_node.uninit)
              decode_group_node(OFFSET, SIZE, BUF, GND, ERR)
              if (ERR.get != types.error.ESUCCESS) {
                debug("persistence: read_gblock_nodes could not decode node at offset " + (toStr(OFFSET) + (" in LEB " + toStr(LNUM))))
              } else {
                NODELIST += GND.get
                ADRLIST += types.address.at(LNUM, OFFSET, SIZE)
              }
            }
          }
          if (ERR.get == types.error.ESUCCESS) {
            OFFSET = OFFSET + SIZE
          }
        }
      }
    }
    if (ERR.get == types.error.ENOENT) {
      ERR := types.error.ESUCCESS
    }
  }

  override def read_gnd(ADR: address, GND: Ref[group_node], ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(ADR.size).fill(0.toByte)
    awbuf.read_buf(ADR.lnum, ADR.pos, ADR.size, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      decode_group_node(0, ADR.size, BUF, GND, ERR)
    }
  }

  override def read_ind(ADR: address, IND: Ref[index_node], ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(ADR.size).fill(0.toByte)
    awbuf.read_buf(ADR.lnum, ADR.pos, ADR.size, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      decode_index_node(0, ADR.size, BUF, IND, ERR)
    }
  }

  override def recover(WROOTADR0: Ref[address], WMAXINO0: Ref[Int], LOG0: nat_list, ORPHANS: nat_set, ERR: Ref[error]): Unit = {
    awbuf.recover(WROOTADR0, WMAXINO0, ORPHANS, LOG0, LPT, ERR)
    Gcarray.allocate(LPT.length, 0)
    LOG := LOG0.deepCopy
    if (ERR.get != types.error.ESUCCESS) {
      debug("persistence: wbuf recover failed")
    } else {
      
      {
        val mode: Ref[Int] = Ref[Int](LEBSIZE)
        awbuf.get_leb_size(mode)
        LEBSIZE = mode.get
      }
      val LOG0: nat_list = LOG.deepCopy
      replay_log(LOG0)
      replay_lpt()
      create_freelist()
      create_gcheap()
    }
  }

  def replay_log(LOG0: nat_list): Unit = {
    while (! LOG0.isEmpty) {
      val N: Int = LOG0.head
      LPT(N).flags = types.lpropflags.LP_GROUP_NODES
      LPT(N).size = LEBSIZE
      LOG0.removeHead
    }
  }

  def replay_lpt(): Unit = {
    var N: Int = 0
    while (N < LPT.length) {
      if (LPT(N).flags != types.lpropflags.LP_FREE && ! LOG.contains(N)) {
        if (LPT(N).flags == types.lpropflags.LP_INDEX_NODES) {
          LPT(N).size = LEBSIZE
        }
        if (LPT(N).ref_size == 0) {
          LPT(N).flags = types.lpropflags.LP_FREE
          LPT(N).size = 0
          awbuf.unmap(N)
        }
      }
      N = N + 1
    }
  }

  override def requires_commit(COMMIT_ : Ref[Boolean]): Unit = {
    awbuf.requires_commit(COMMIT_)
  }

  override def set_gblock_refsize(LNUM: Int, N: Int): Unit = {
    LPT(LNUM).ref_size = N
    if (! LOG.contains(LNUM)) {
      gc_heap_update(Gcarray(LNUM), N, GCHEAP, Gcarray)
    }
  }

  override def set_iblock_refsize(LNUM: Int, N: Int): Unit = {
    LPT(LNUM).ref_size = N
  }

  override def sync(ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val WBUFLEB0 = Ref[bufleb](types.bufleb.uninit)
    awbuf.get_buf(WBUFLEB0)
    val AROFS0 = Ref[Boolean](helpers.scala.Boolean.uninit)
    awbuf.is_readonly(AROFS0)
    val PAGESIZE = Ref[Int](0)
    awbuf.get_page_size(PAGESIZE)
    if (AROFS0.get != true) {
      if (WBUFLEB0.get.isInstanceOf[types.bufleb.buffered]) {
        val OFFSET: Int = LPT(WBUFLEB0.get.leb).size
        if (! is_aligned(OFFSET, PAGESIZE.get)) {
          val SIZE: Int = PAGESIZE.get - OFFSET % PAGESIZE.get
          val BUF: buffer = new buffer(SIZE).fill(0.toByte)
          persistence_encode_padding_node(BUF, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            awbuf.write_buf(BUF.length, BUF, ERR)
            if (ERR.get == types.error.ESUCCESS) {
              LPT(WBUFLEB0.get.leb).size = LPT(WBUFLEB0.get.leb).size + SIZE
            } else {
              debug("persistence: flushing of LEB " + (toStr(WBUFLEB0.get.leb) + " failed"))
              LPT(WBUFLEB0.get.leb).size = LEBSIZE
              awbuf.enter_readonly()
            }
          } else {
            debug("persistence: could not encode padding node")
            awbuf.enter_readonly()
          }
        }
      }
    } else {
      ERR := types.error.EROFS
    }
  }

}
