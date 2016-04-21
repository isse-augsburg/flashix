// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import encoding.group_node._
import encoding.index_node._
import encoding.node_header._
import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import proc._
import types._
import types.error.error
import types.lpropflags.lpropflags

class persistence_asm(val GCHEAP : binheap, val LOG : nat_list, val FREELIST : nat_list, val LPT : lp_array, val apersistence_io : apersistence_io_interface)(implicit _algebraic_implicit: algebraic.Algebraic, _procedures_implicit: proc.Procedures) extends apersistence_interface {
  import _algebraic_implicit._
  import _procedures_implicit._

  override def apersistence_add_gnd(LNUM: Int, GND: group_node, ADR: Ref[address], ERR: Ref[error]): Unit = {
    if (LPT(LNUM).size + flashsize(GND) > LEB_SIZE)
      ERR := types.error.ENOSPC
    else {
      persistence_ensure_buffered(LNUM)
      val SIZE = new Ref[Int](0)
      val BUF: buffer = new buffer(flashsize(GND)).fill(0.toByte)
      encode_group_node(GND, 0, SIZE, BUF, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        apersistence_io.apersistence_io_write_buf(LNUM, SIZE.get, BUF, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        ADR := types.address.at(LNUM, LPT(LNUM).size, SIZE.get)
        LPT(LNUM).size = LPT(LNUM).size + SIZE.get
      } else {
        debug("persistence: adding group node to LEB " + (toStr(LNUM) + " failed"))
        LPT(LNUM).size = LEB_SIZE
      }
    }
  }

  override def apersistence_add_gnds(LNUM: Int, NODELIST: group_node_list, ADRLIST: address_list, ERR: Ref[error]): Unit = {
    if (NODELIST.isEmpty) {
      ADRLIST.clear
      ERR := types.error.ESUCCESS
    } else {
      persistence_ensure_buffered(LNUM)
      val BUF: buffer = new buffer()
      persistence_encode_group_nodes(LNUM, LPT(LNUM).size, NODELIST, BUF, ADRLIST, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        apersistence_io.apersistence_io_write_buf(LNUM, BUF.length, BUF, ERR)
      }
      if (ERR.get == types.error.ESUCCESS)
        LPT(LNUM).size = LPT(LNUM).size + BUF.length
      else {
        debug("persistence: adding index node to LEB " + (toStr(LNUM) + " failed"))
        LPT(LNUM).size = LEB_SIZE
      }
    }
  }

  override def apersistence_add_ind(LNUM: Int, IND: index_node, ADR: Ref[address], ERR: Ref[error]): Unit = {
    if (LPT(LNUM).size + flashsize(IND) > LEB_SIZE)
      ERR := types.error.ENOSPC
    else {
      persistence_ensure_buffered(LNUM)
      val SIZE = new Ref[Int](0)
      val BUF: buffer = new buffer(flashsize(IND)).fill(0.toByte)
      encode_index_node(IND, 0, SIZE, BUF, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        apersistence_io.apersistence_io_write_buf(LNUM, SIZE.get, BUF, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        ADR := types.address.at(LNUM, LPT(LNUM).size, SIZE.get)
        LPT(LNUM).size = LPT(LNUM).size + SIZE.get
      } else
        LPT(LNUM).size = LEB_SIZE
    }
  }

  override def apersistence_allocate_gnd(N: Ref[Int], ERR: Ref[error]): Unit = {
    persistence_allocate_leb(N, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      apersistence_io.apersistence_io_add_log_leb(N.get, ERR)
      if (ERR.get != types.error.ESUCCESS)
        N.get +=: FREELIST
      
    }
    if (ERR.get == types.error.ESUCCESS) {
      LPT(N.get).flags = types.lpropflags.LP_GROUP_NODES
      LOG += N.get
    }
  }

  override def apersistence_allocate_ind(N: Ref[Int], ERR: Ref[error]): Unit = {
    persistence_allocate_leb(N, ERR)
    if (ERR.get == types.error.ESUCCESS)
      LPT(N.get).flags = types.lpropflags.LP_INDEX_NODES
    
  }

  override def apersistence_commit(PROOTADR0: address, PMAXINO0: Int, ORPHANS0: nat_set, ERR: Ref[error]): Unit = {
    persistence_cleanup_bufs()
    apersistence_io.apersistence_io_commit(LPT, PROOTADR0, PMAXINO0, ORPHANS0, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      persistence_gc_heap_add_log()
      persistence_cleanup_lpt()
    }
  }

  override def apersistence_deallocate_gnd(N: Int, ERR: Ref[error]): Unit = {
    apersistence_io.apersistence_io_unmap(N)
    LPT(N).flags = types.lpropflags.LP_FREE
    LPT(N).ref_size = 0
    LPT(N).size = 0
    FREELIST += N
    gc_heap_remove(LPT(N).gcheapidx, GCHEAP, LPT)
    ERR := types.error.ESUCCESS
  }

  override def apersistence_flush_gnd(LNUM: Int, ERR: Ref[error]): Unit = {
    persistence_flush(LNUM, ERR)
  }

  override def apersistence_flush_ind(LNUM: Int, ERR: Ref[error]): Unit = {
    persistence_flush(LNUM, ERR)
  }

  override def apersistence_format(VOLSIZE: Int, PMAXINO0: Int, ERR: Ref[error]): Unit = {
    LPT.allocate(VOLSIZE, types.lprops.uninit)
    LPT.fill(types.lprops.mklp(0, 0, types.lpropflags.LP_FREE, 0))
    val ADR: address = types.address.uninit
    apersistence_io.apersistence_io_format(VOLSIZE, LPT, ADR, PMAXINO0, ERR)
    if (ERR.get != types.error.ESUCCESS) {
      debug("persistence: persistence-io format failed")
    } else {
      LOG.clear
      persistence_create_freelist()
      gc_heap_empty(GCHEAP)
    }
  }

  override def apersistence_get_gblock_refsize(LNUM: Int, N: Ref[Int]): Unit = {
    N := LPT(LNUM).ref_size
  }

  override def apersistence_get_gblock_size(LNUM: Int, N: Ref[Int]): Unit = {
    N := LPT(LNUM).size
  }

  override def apersistence_get_gc_block(N: Ref[Int], ERR: Ref[error]): Unit = {
    val empty_ = new Ref[Boolean](helpers.scala.Boolean.uninit)
    gc_heap_is_empty(GCHEAP, empty_)
    if (empty_.get)
      ERR := types.error.ENOSPC
    else {
      gc_heap_get_min(GCHEAP, N)
      ERR := types.error.ESUCCESS
    }
  }

  override def apersistence_get_iblock_refsize(LNUM: Int, N: Ref[Int]): Unit = {
    N := LPT(LNUM).ref_size
  }

  override def apersistence_get_iblock_size(LNUM: Int, N: Ref[Int]): Unit = {
    N := LPT(LNUM).size
  }

  override def apersistence_read_gblock_nodes(LNUM: Int, ADRLIST: address_list, NODELIST: group_node_list, ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(LEB_SIZE).fill(0.toByte)
    apersistence_io.apersistence_io_read_buf(LNUM, 0, LEB_SIZE, BUF, ERR)
    var OFFSET: Int = 0
    while (ERR.get == types.error.ESUCCESS && OFFSET < LEB_SIZE) {
      if (! (OFFSET + NODE_HEADER_SIZE <= BUF.length) || isempty(BUF, OFFSET, NODE_HEADER_SIZE))
        ERR := types.error.ENOENT
      else {
        val NDHD = new Ref[node_header](types.node_header.uninit)
        decode_header(OFFSET, BUF, NDHD, ERR)
        if (ERR.get != types.error.ESUCCESS) {
          debug("persistence: read_gblock_nodes could not decode header in LEB " + (toStr(LNUM) + (" at offset " + toStr(OFFSET))))
        } else {
          val SIZE: Int = 2 * NODE_HEADER_SIZE + alignUp(NDHD.get.size, 2 * NODE_HEADER_SIZE)
          if (OFFSET + SIZE > LEB_SIZE) {
            debug("persistence: read_gblock_nodes ignoring node of size " + (toStr(SIZE) + (" at offset " + (toStr(OFFSET) + (" in LEB " + toStr(LNUM))))))
            ERR := types.error.ENOENT
          } else           if (! NDHD.get.ispadding) {
            val GND = new Ref[group_node](types.group_node.uninit)
            decode_group_node(OFFSET, SIZE, BUF, GND, ERR)
            if (ERR.get != types.error.ESUCCESS) {
              debug("persistence: read_gblock_nodes could not decode node at offset " + (toStr(OFFSET) + (" in LEB " + toStr(LNUM))))
            } else {
              NODELIST += GND.get
              ADRLIST += types.address.at(LNUM, OFFSET, SIZE)
            }
          } else           if (! rangeeq(BUF, OFFSET + (NODE_HEADER_SIZE + alignUp(NDHD.get.size, 2 * NODE_HEADER_SIZE)), validtrailer, 0, NODE_HEADER_SIZE))
            ERR := types.error.ENOENT
          
          if (ERR.get == types.error.ESUCCESS)
            OFFSET = OFFSET + SIZE
          
        }
      }
    }
    if (ERR.get == types.error.ENOENT)
      ERR := types.error.ESUCCESS
    
  }

  override def apersistence_read_gnd(ADR: address, GND: Ref[group_node], ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(ADR.size).fill(0.toByte)
    apersistence_io.apersistence_io_read_buf(ADR.lnum, ADR.pos, ADR.size, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      decode_group_node(0, ADR.size, BUF, GND, ERR)
    }
  }

  override def apersistence_read_ind(ADR: address, IND: index_node, ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(ADR.size).fill(0.toByte)
    apersistence_io.apersistence_io_read_buf(ADR.lnum, ADR.pos, ADR.size, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      decode_index_node(0, ADR.size, BUF, IND, ERR)
    }
  }

  override def apersistence_recover(PROOTADR0: Ref[address], PMAXINO0: Ref[Int], LOG0: nat_list, ORPHANS: nat_set, ERR: Ref[error]): Unit = {
    apersistence_io.apersistence_io_recover(PROOTADR0, PMAXINO0, ORPHANS, LOG0, LPT, ERR)
    LOG := LOG0.deepCopy
    if (ERR.get != types.error.ESUCCESS) {
      debug("persistence: persistence-io format failed")
    } else {
      val LOG0: nat_list = LOG.deepCopy
      persistence_replay_log(LOG0)
      persistence_replay_lpt()
      persistence_create_freelist()
      persistence_create_gcheap()
    }
  }

  override def apersistence_requires_commit(COMMIT_ : Ref[Boolean]): Unit = {
    apersistence_io.apersistence_io_requires_commit(COMMIT_)
  }

  override def apersistence_set_gblock_refsize(LNUM: Int, N: Int): Unit = {
    LPT(LNUM).ref_size = N
    if (! LOG.contains(LNUM)) {
      gc_heap_update(LPT(LNUM).gcheapidx, N, GCHEAP, LPT)
    }
  }

  override def apersistence_set_iblock_refsize(LNUM: Int, N: Int): Unit = {
    LPT(LNUM).ref_size = N
  }

  private def bin_min_heap_bubble_down(n0: Int, bh: binheap, lpt: lp_array): Unit = {
    var n1: Int = n0
    while (2 * n1 + 1 < bh.size && (bh.ar(2 * n1 + 1).key < bh.ar(n1).key || 2 * n1 + 2 < bh.size && bh.ar(2 * n1 + 2).key < bh.ar(n1).key)) {
      val ki: keyindex = bh.ar(n1).deepCopy
      val ki0: keyindex = bh.ar(2 * n1 + 1).deepCopy
      if (2 * n1 + 2 < bh.size && bh.ar(2 * n1 + 2).key < ki0.key) {
        val ki1: keyindex = bh.ar(2 * n1 + 2).deepCopy
        bh.ar(n1) = ki1.deepCopy
        lpt(ki1.idx).gcheapidx = n1
        bh.ar(2 * n1 + 2) = ki.deepCopy
        lpt(ki.idx).gcheapidx = 2 * n1 + 2
        n1 = 2 * n1 + 2
      } else {
        bh.ar(n1) = ki0.deepCopy
        lpt(ki0.idx).gcheapidx = n1
        bh.ar(2 * n1 + 1) = ki.deepCopy
        lpt(ki.idx).gcheapidx = 2 * n1 + 1
        n1 = 2 * n1 + 1
      }
    }
  }

  private def bin_min_heap_bubble_up(n0: Int, bh: binheap, lpt: lp_array): Unit = {
    var n1: Int = n0
    while (n1 != 0 && bh.ar(n1).key < bh.ar((n1 - 1) / 2).key) {
      val ki: keyindex = bh.ar(n1).deepCopy
      val ki0: keyindex = bh.ar((n1 - 1) / 2).deepCopy
      bh.ar(n1) = ki0.deepCopy
      lpt(ki0.idx).gcheapidx = n1
      n1 = (n1 - 1) / 2
      bh.ar(n1) = ki.deepCopy
      lpt(ki.idx).gcheapidx = n1
    }
  }

  private def bin_min_heap_next_2pot(n0: Int, n1: Ref[Int]): Unit = {
    while (n1.get < n0) {
      n1 := n1.get * 2
    }
  }

  private def bin_min_heap_resize(n1: Int, bh: binheap): Unit = {
    if (n1 > bh.size) {
      val n0 = new Ref[Int](bh.ar.length)
      bin_min_heap_next_2pot(n1, n0)
      val kar: key_array = new key_array(n0.get).fill(types.keyindex.uninit)
      kar.copy(bh.ar, 0, 0, bh.size)
      bh.ar = kar
    }
  }

  private def decode_group_node(OFFSET: Int, FD: Int, BUF: buffer, GND: Ref[group_node], ERR: Ref[error]): Unit = {
    val SIZE = new Ref[Int](FD)
    if (SIZE.get < 2 * NODE_HEADER_SIZE) {
      debug("persistence: decode_node expected size wrong")
      ERR := types.error.EINVAL
    } else     if (! (OFFSET + SIZE.get <= BUF.length)) {
      debug("persistence: decode_node buffer insufficient")
      ERR := types.error.EINVAL
    } else {
      val NDHD = new Ref[node_header](types.node_header.uninit)
      decode_header(OFFSET, BUF, NDHD, ERR)
      if (ERR.get == types.error.ESUCCESS && NDHD.get.ispadding) {
        debug("persistence: decode_node got padding node")
        ERR := types.error.EIO
      }
      if (ERR.get == types.error.ESUCCESS && alignUp(NDHD.get.size, 2 * NODE_HEADER_SIZE) + 2 * NODE_HEADER_SIZE != SIZE.get) {
        debug("persistence: decode_node node of wrong size")
        ERR := types.error.EIO
      }
      if (ERR.get == types.error.ESUCCESS) {
        decode_group_node_headerless(OFFSET + NODE_HEADER_SIZE, BUF, GND, SIZE, ERR)
        if (ERR.get != types.error.ESUCCESS) {
          debug("persistence: decode_node decoding failed")
        } else         if (SIZE.get != NDHD.get.size) {
          debug("persistence: decode_node size of node unexpected")
          ERR := types.error.EINVAL
        }
      }
      if (ERR.get == types.error.ESUCCESS && ! rangeeq(BUF, OFFSET + (NODE_HEADER_SIZE + alignUp(NDHD.get.size, 2 * NODE_HEADER_SIZE)), validtrailer, 0, NODE_HEADER_SIZE)) {
        debug("persistence: decode_node no validtrailer")
        ERR := types.error.EIO
      }
    }
  }

  private def decode_header(n: Int, buf: buffer, ndhd: Ref[node_header], err: Ref[error]): Unit = {
    if (isempty(buf, n, NODE_HEADER_SIZE))
      err := types.error.EINVAL
    else {
      val m = new Ref[Int](0)
      decode_header_empty(n, buf, ndhd, m, err)
    }
  }

  private def decode_index_node(OFFSET: Int, FD: Int, BUF: buffer, IND: index_node, ERR: Ref[error]): Unit = {
    val SIZE = new Ref[Int](FD)
    if (SIZE.get < 2 * NODE_HEADER_SIZE) {
      debug("persistence: decode_node expected size wrong")
      ERR := types.error.EINVAL
    } else     if (! (OFFSET + SIZE.get <= BUF.length)) {
      debug("persistence: decode_node buffer insufficient")
      ERR := types.error.EINVAL
    } else {
      val NDHD = new Ref[node_header](types.node_header.uninit)
      decode_header(OFFSET, BUF, NDHD, ERR)
      if (ERR.get == types.error.ESUCCESS && NDHD.get.ispadding) {
        debug("persistence: decode_node got padding node")
        ERR := types.error.EIO
      }
      if (ERR.get == types.error.ESUCCESS && alignUp(NDHD.get.size, 2 * NODE_HEADER_SIZE) + 2 * NODE_HEADER_SIZE != SIZE.get) {
        debug("persistence: decode_node node of wrong size")
        ERR := types.error.EIO
      }
      if (ERR.get == types.error.ESUCCESS) {
        decode_index_node_headerless(OFFSET + NODE_HEADER_SIZE, BUF, IND, SIZE, ERR)
        if (ERR.get != types.error.ESUCCESS) {
          debug("persistence: decode_node decoding failed")
        } else         if (SIZE.get != NDHD.get.size) {
          debug("persistence: decode_node size of node unexpected")
          ERR := types.error.EINVAL
        }
      }
      if (ERR.get == types.error.ESUCCESS && ! rangeeq(BUF, OFFSET + (NODE_HEADER_SIZE + alignUp(NDHD.get.size, 2 * NODE_HEADER_SIZE)), validtrailer, 0, NODE_HEADER_SIZE)) {
        debug("persistence: decode_node no validtrailer")
        ERR := types.error.EIO
      }
    }
  }

  private def encode_group_node(GND: group_node, OFFSET: Int, SIZE: Ref[Int], BUF: buffer, ERR: Ref[error]): Unit = {
    val NDSIZE: Int = group_node_size_headerless(GND)
    val ALIGNEDSIZE: Int = alignUp(NDSIZE, 2 * NODE_HEADER_SIZE)
    encode_header(types.node_header.nodeheader(NDSIZE, false), OFFSET, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val SIZE0 = new Ref[Int](0)
      encode_group_node_headerless(GND, OFFSET + NODE_HEADER_SIZE, BUF, SIZE0, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      BUF.fill(empty, OFFSET + (NODE_HEADER_SIZE + NDSIZE), ALIGNEDSIZE - NDSIZE)
      BUF.copy(validtrailer, 0, OFFSET + (NODE_HEADER_SIZE + ALIGNEDSIZE), NODE_HEADER_SIZE)
    }
    SIZE := 2 * NODE_HEADER_SIZE + ALIGNEDSIZE
  }

  private def encode_header(ndhd: node_header, n: Int, buf: buffer, err: Ref[error]): Unit = {
    val m = new Ref[Int](0)
    encode_header_empty(ndhd, n, buf, m, err)
    if (err.get == types.error.ESUCCESS && isempty(buf, n, m.get)) {
      debug("encoding_nonempty: encoding is empty")
      err := types.error.EINVAL
    }
  }

  private def encode_index_node(IND: index_node, OFFSET: Int, SIZE: Ref[Int], BUF: buffer, ERR: Ref[error]): Unit = {
    val NDSIZE: Int = index_node_size_headerless(IND)
    val ALIGNEDSIZE: Int = alignUp(NDSIZE, 2 * NODE_HEADER_SIZE)
    encode_header(types.node_header.nodeheader(NDSIZE, false), OFFSET, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val SIZE0 = new Ref[Int](0)
      encode_index_node_headerless(IND, OFFSET + NODE_HEADER_SIZE, BUF, SIZE0, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      BUF.fill(empty, OFFSET + (NODE_HEADER_SIZE + NDSIZE), ALIGNEDSIZE - NDSIZE)
      BUF.copy(validtrailer, 0, OFFSET + (NODE_HEADER_SIZE + ALIGNEDSIZE), NODE_HEADER_SIZE)
    }
    SIZE := 2 * NODE_HEADER_SIZE + ALIGNEDSIZE
  }

  private def gc_heap_empty(bh: binheap): Unit = {
    bh := types.binheap.bin_heap(new key_array(1).fill(types.keyindex.uninit), 0)
  }

  private def gc_heap_get_min(bh: binheap, n0: Ref[Int]): Unit = {
    if (bh.size == 0)
      assert(false, """abort""")
    
    n0 := bh.ar(0).idx
  }

  private def gc_heap_insert(ki: keyindex, bh: binheap, lpt: lp_array): Unit = {
    if (! (ki.idx < lpt.length))
      assert(false, """abort""")
    
    bin_min_heap_resize(bh.size + 1, bh)
    bh.ar(bh.size) = ki.deepCopy
    lpt(ki.idx).gcheapidx = bh.size
    bh.size = bh.size + 1
    bin_min_heap_bubble_up(bh.size - 1, bh, lpt)
  }

  private def gc_heap_is_empty(bh: binheap, empty_ : Ref[Boolean]): Unit = {
    empty_ := (bh.size == 0)
  }

  private def gc_heap_remove(n0: Int, bh: binheap, lpt: lp_array): Unit = {
    if (! (n0 < bh.size))
      assert(false, """abort""")
    
    bh.size = bh.size - 1
    if (bh.size != 0 && bh.size != n0) {
      bh.ar(n0) = bh.ar(bh.size).deepCopy
      lpt(bh.ar(n0).idx).gcheapidx = n0
      if (n0 != 0 && bh.ar(n0).key < bh.ar((n0 - 1) / 2).key) {
        bin_min_heap_bubble_up(n0, bh, lpt)
      } else {
        bin_min_heap_bubble_down(n0, bh, lpt)
      }
    }
  }

  private def gc_heap_update(n2: Int, n0: Int, bh: binheap, lpt: lp_array): Unit = {
    if (! (n2 < bh.size))
      assert(false, """abort""")
    
    val n1: Int = bh.ar(n2).key
    bh.ar(n2).key = n0
    if (n0 < n1) {
      bin_min_heap_bubble_up(n2, bh, lpt)
    } else {
      bin_min_heap_bubble_down(n2, bh, lpt)
    }
  }

  private def persistence_allocate_leb(N: Ref[Int], ERR: Ref[error]): Unit = {
    if (FREELIST.isEmpty)
      ERR := types.error.ENOSPC
    else {
      N := FREELIST.head
      apersistence_io.apersistence_io_remap(N.get, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        FREELIST.removeHead
        LPT(N.get).size = 0
        LPT(N.get).ref_size = 0
      }
    }
  }

  private def persistence_cleanup_bufs(): Unit = {
    val WBS0: nat_set = new nat_set()
    apersistence_io.apersistence_io_get_bufs(WBS0)
    while (! WBS0.isEmpty) {
      val LNUM: Int = WBS0.head
      WBS0 -= LNUM
      LPT(LNUM).size = LEB_SIZE
    }
    apersistence_io.apersistence_io_destroy_bufs()
  }

  private def persistence_cleanup_lpt(): Unit = {
    var N: Int = 0
    while (N < LPT.length) {
      if (LPT(N).ref_size == 0 && LPT(N).flags != types.lpropflags.LP_FREE) {
        if (LPT(N).flags == types.lpropflags.LP_GROUP_NODES) {
          gc_heap_remove(LPT(N).gcheapidx, GCHEAP, LPT)
        }
        LPT(N).flags = types.lpropflags.LP_FREE
        LPT(N).size = 0
        FREELIST += N
        apersistence_io.apersistence_io_unmap(N)
      }
      N = N + 1
    }
  }

  private def persistence_create_freelist(): Unit = {
    FREELIST.clear
    var N: Int = 0
    while (N < LPT.length) {
      if (LPT(N).flags == types.lpropflags.LP_FREE)
        FREELIST += N
      
      N = N + 1
    }
  }

  private def persistence_create_gcheap(): Unit = {
    gc_heap_empty(GCHEAP)
    var N: Int = 0
    while (N < LPT.length) {
      if (LPT(N).flags == types.lpropflags.LP_GROUP_NODES && ! LOG.contains(N)) {
        val KI: keyindex = types.keyindex.key_index(LPT(N).ref_size, N)
        gc_heap_insert(KI, GCHEAP, LPT)
      }
      N = N + 1
    }
  }

  private def persistence_encode_group_nodes(LNUM: Int, N: Int, GNDLIST: group_node_list, BUF: buffer, ADRLIST: address_list, ERR: Ref[error]): Unit = {
    val NODELIST: group_node_list = GNDLIST.deepCopy
    ERR := types.error.ESUCCESS
    BUF.allocate(flashsize(NODELIST), 0.toByte)
    ADRLIST.clear
    var OFFSET: Int = 0
    val SIZE = new Ref[Int](0)
    while (ERR.get == types.error.ESUCCESS && ! NODELIST.isEmpty) {
      encode_group_node(NODELIST.head, OFFSET, SIZE, BUF, ERR)
      ADRLIST += types.address.at(LNUM, N + OFFSET, SIZE.get)
      OFFSET = OFFSET + SIZE.get
      NODELIST.removeHead
    }
  }

  private def persistence_encode_padding_node(BUF: buffer, ERR: Ref[error]): Unit = {
    val SIZE: Int = BUF.length - 2 * NODE_HEADER_SIZE
    encode_header(types.node_header.nodeheader(SIZE, true), 0, BUF, ERR)
    BUF.copy(validtrailer, 0, BUF.length - NODE_HEADER_SIZE, NODE_HEADER_SIZE)
  }

  private def persistence_ensure_buffered(LNUM: Int): Unit = {
    val ISBUF = new Ref[Boolean](helpers.scala.Boolean.uninit)
    apersistence_io.apersistence_io_is_buffered(LNUM, ISBUF)
    if (ISBUF.get != true) {
      apersistence_io.apersistence_io_create_buf(LNUM, LPT(LNUM).size)
    }
  }

  private def persistence_flush(LNUM: Int, ERR: Ref[error]): Unit = {
    val OFFSET: Int = LPT(LNUM).size
    if (is_aligned(OFFSET, EB_PAGE_SIZE))
      ERR := types.error.ESUCCESS
    else {
      val ISBUF = new Ref[Boolean](helpers.scala.Boolean.uninit)
      apersistence_io.apersistence_io_is_buffered(LNUM, ISBUF)
      if (ISBUF.get) {
        val SIZE: Int = EB_PAGE_SIZE - OFFSET % EB_PAGE_SIZE
        val BUF: buffer = new buffer(SIZE).fill(0.toByte)
        persistence_encode_padding_node(BUF, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          apersistence_io.apersistence_io_write_buf(LNUM, BUF.length, BUF, ERR)
          if (ERR.get != types.error.ESUCCESS) {
            debug("persistence: flushing of LEB " + (toStr(LNUM) + " failed"))
          }
        }
        if (ERR.get == types.error.ESUCCESS)
          LPT(LNUM).size = LPT(LNUM).size + SIZE
        else
          LPT(LNUM).size = LEB_SIZE
      } else
        ERR := types.error.ESUCCESS
    }
  }

  private def persistence_gc_heap_add_log(): Unit = {
    while (! LOG.isEmpty) {
      val LNUM: Int = LOG.head
      val KI: keyindex = types.keyindex.key_index(LPT(LNUM).ref_size, LNUM)
      gc_heap_insert(KI, GCHEAP, LPT)
      LOG.removeHead
    }
  }

  private def persistence_replay_log(LOG0: nat_list): Unit = {
    while (! LOG0.isEmpty) {
      val N: Int = LOG0.head
      LPT(N).flags = types.lpropflags.LP_GROUP_NODES
      LPT(N).size = LEB_SIZE
      LOG0.removeHead
    }
  }

  private def persistence_replay_lpt(): Unit = {
    var N: Int = 0
    while (N < LPT.length) {
      if (LPT(N).flags != types.lpropflags.LP_FREE && ! LOG.contains(N)) {
        if (LPT(N).flags == types.lpropflags.LP_INDEX_NODES)
          LPT(N).size = LEB_SIZE
        
        if (LPT(N).ref_size == 0) {
          LPT(N).flags = types.lpropflags.LP_FREE
          LPT(N).size = 0
          apersistence_io.apersistence_io_unmap(N)
        }
      }
      N = N + 1
    }
  }

}

