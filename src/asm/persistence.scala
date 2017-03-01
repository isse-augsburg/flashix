// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
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

class persistence_asm(val FREELIST : nat_list, val GCHEAP : binheap, val LOG : nat_list, val LPT : lp_array, val awbuf : awbuf_interface)(implicit _algebraic_implicit: algebraic.Algebraic, _procedures_implicit: proc.Procedures) extends apersistence_interface {
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
      encode_group_nodes(WBUFLEB0.get.leb, LPT(WBUFLEB0.get.leb).size, NODELIST, BUF, ADRLIST, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (! (LPT(WBUFLEB0.get.leb).size + BUF.length <= LEB_SIZE)) {
          debug("persistence: trying to write beyond LEB size")
          awbuf.enter_readonly()
          ERR := types.error.ENOSPC
        } else {
          awbuf.write_buf(BUF.length, BUF, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            LPT(WBUFLEB0.get.leb).size = LPT(WBUFLEB0.get.leb).size + BUF.length
          } else {
            debug("persistence: adding group node to LEB " + (toStr(WBUFLEB0.get.leb) + " failed"))
            LPT(WBUFLEB0.get.leb).size = LEB_SIZE
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
        if (! (LPT(WBUFLEB0.get.leb).size + SIZE.get <= LEB_SIZE)) {
          debug("persistence: trying to write beyond LEB size")
          awbuf.enter_readonly()
          ERR := types.error.ENOSPC
        } else {
          awbuf.write_buf(SIZE.get, BUF, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            ADR := types.address.at(WBUFLEB0.get.leb, LPT(WBUFLEB0.get.leb).size, SIZE.get)
            LPT(WBUFLEB0.get.leb).size = LPT(WBUFLEB0.get.leb).size + SIZE.get
          } else {
            LPT(WBUFLEB0.get.leb).size = LEB_SIZE
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

  def bin_min_heap_bubble_down(n: Int, bh: binheap, lpt: lp_array): Unit = {
    var m: Int = n
    while (2 * m + 1 < bh.size && (bh.ar(2 * m + 1).key < bh.ar(m).key || 2 * m + 2 < bh.size && bh.ar(2 * m + 2).key < bh.ar(m).key)) {
      val ki: keyindex = bh.ar(m).deepCopy
      val ki0: keyindex = bh.ar(2 * m + 1).deepCopy
      if (2 * m + 2 < bh.size && bh.ar(2 * m + 2).key < ki0.key) {
        val ki1: keyindex = bh.ar(2 * m + 2).deepCopy
        bh.ar(m) = ki1.deepCopy
        lpt(ki1.idx).gcheapidx = m
        bh.ar(2 * m + 2) = ki.deepCopy
        lpt(ki.idx).gcheapidx = 2 * m + 2
        m = 2 * m + 2
      } else {
        bh.ar(m) = ki0.deepCopy
        lpt(ki0.idx).gcheapidx = m
        bh.ar(2 * m + 1) = ki.deepCopy
        lpt(ki.idx).gcheapidx = 2 * m + 1
        m = 2 * m + 1
      }
    }
  }

  def bin_min_heap_bubble_up(n: Int, bh: binheap, lpt: lp_array): Unit = {
    var m: Int = n
    while (m != 0 && bh.ar(m).key < bh.ar((m - 1) / 2).key) {
      val ki: keyindex = bh.ar(m).deepCopy
      val ki0: keyindex = bh.ar((m - 1) / 2).deepCopy
      bh.ar(m) = ki0.deepCopy
      lpt(ki0.idx).gcheapidx = m
      m = (m - 1) / 2
      bh.ar(m) = ki.deepCopy
      lpt(ki.idx).gcheapidx = m
    }
  }

  def bin_min_heap_next_2pot(minsize: Int, n: Ref[Int]): Unit = {
    while (n.get < minsize) {
      n := n.get * 2
    }
  }

  def bin_min_heap_resize(minsize: Int, bh: binheap): Unit = {
    if (minsize > bh.size) {
      val n = Ref[Int](bh.ar.length)
      bin_min_heap_next_2pot(minsize, n)
      val kar: key_array = new key_array(n.get).fill(types.keyindex.uninit)
      kar.copy(bh.ar, 0, 0, bh.size)
      bh.ar = kar
    }
  }

  def cleanup_lpt(): Unit = {
    var N: Int = 0
    while (N < LPT.length) {
      if (LPT(N).ref_size == 0 && LPT(N).flags != types.lpropflags.LP_FREE) {
        if (LPT(N).flags == types.lpropflags.LP_GROUP_NODES) {
          gc_heap_remove(LPT(N).gcheapidx, GCHEAP, LPT)
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
        gc_heap_insert(KI, GCHEAP, LPT)
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
      gc_heap_remove(LPT(N).gcheapidx, GCHEAP, LPT)
      ERR := types.error.ESUCCESS
    }
  }

  def decode_group_node(OFFSET: Int, N: Int, BUF: buffer, GND: Ref[group_node], ERR: Ref[error]): Unit = {
    val SIZE = Ref[Int](N)
    if (SIZE.get < 2 * NODE_HEADER_SIZE) {
      debug("persistence: decode_node expected size wrong")
      ERR := types.error.EINVAL
    } else     if (! (OFFSET + SIZE.get <= BUF.length)) {
      debug("persistence: decode_node buffer insufficient")
      ERR := types.error.EINVAL
    } else {
      val NDHD = Ref[node_header](types.node_header.uninit)
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

  def decode_header(n: Int, buf: buffer, ndhd: Ref[node_header], err: Ref[error]): Unit = {
    if (isempty(buf, n, NODE_HEADER_SIZE)) {
      err := types.error.EINVAL
    } else {
      val m = Ref[Int](0)
      decode_header_empty(n, buf, ndhd, m, err)
    }
  }

  def decode_index_node(OFFSET: Int, N: Int, BUF: buffer, IND: Ref[index_node], ERR: Ref[error]): Unit = {
    val SIZE = Ref[Int](N)
    if (SIZE.get < 2 * NODE_HEADER_SIZE) {
      debug("persistence: decode_node expected size wrong")
      ERR := types.error.EINVAL
    } else     if (! (OFFSET + SIZE.get <= BUF.length)) {
      debug("persistence: decode_node buffer insufficient")
      ERR := types.error.EINVAL
    } else {
      val NDHD = Ref[node_header](types.node_header.uninit)
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

  def encode_group_node(GND: group_node, OFFSET: Int, SIZE: Ref[Int], BUF: buffer, ERR: Ref[error]): Unit = {
    val NDSIZE: Int = group_node_size_headerless(GND)
    val ALIGNEDSIZE: Int = alignUp(NDSIZE, 2 * NODE_HEADER_SIZE)
    encode_header(types.node_header.nodeheader(NDSIZE, false), OFFSET, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val SIZE0 = Ref[Int](0)
      encode_group_node_headerless(GND, OFFSET + NODE_HEADER_SIZE, BUF, SIZE0, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      BUF.fill(empty, OFFSET + (NODE_HEADER_SIZE + NDSIZE), ALIGNEDSIZE - NDSIZE)
      BUF.copy(validtrailer, 0, OFFSET + (NODE_HEADER_SIZE + ALIGNEDSIZE), NODE_HEADER_SIZE)
    }
    SIZE := 2 * NODE_HEADER_SIZE + ALIGNEDSIZE
  }

  def encode_group_nodes(LNUM: Int, N: Int, GNDLIST: group_node_list, BUF: buffer, ADRLIST: address_list, ERR: Ref[error]): Unit = {
    val NODELIST: group_node_list = GNDLIST.deepCopy
    ERR := types.error.ESUCCESS
    BUF.allocate(flashsize(NODELIST), 0.toByte)
    ADRLIST.clear
    var OFFSET: Int = 0
    val SIZE = Ref[Int](0)
    while (ERR.get == types.error.ESUCCESS && ! NODELIST.isEmpty) {
      encode_group_node(NODELIST.head, OFFSET, SIZE, BUF, ERR)
      ADRLIST += types.address.at(LNUM, N + OFFSET, SIZE.get)
      OFFSET = OFFSET + SIZE.get
      NODELIST.removeHead
    }
  }

  def encode_header(ndhd: node_header, n: Int, buf: buffer, err: Ref[error]): Unit = {
    val m = Ref[Int](0)
    encode_header_empty(ndhd, n, buf, m, err)
    if (err.get == types.error.ESUCCESS && isempty(buf, n, m.get)) {
      debug("encoding_nonempty: encoding is empty")
      err := types.error.EINVAL
    }
  }

  def encode_index_node(IND: index_node, OFFSET: Int, SIZE: Ref[Int], BUF: buffer, ERR: Ref[error]): Unit = {
    val NDSIZE: Int = index_node_size_headerless(IND)
    val ALIGNEDSIZE: Int = alignUp(NDSIZE, 2 * NODE_HEADER_SIZE)
    encode_header(types.node_header.nodeheader(NDSIZE, false), OFFSET, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val SIZE0 = Ref[Int](0)
      encode_index_node_headerless(IND, OFFSET + NODE_HEADER_SIZE, BUF, SIZE0, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      BUF.fill(empty, OFFSET + (NODE_HEADER_SIZE + NDSIZE), ALIGNEDSIZE - NDSIZE)
      BUF.copy(validtrailer, 0, OFFSET + (NODE_HEADER_SIZE + ALIGNEDSIZE), NODE_HEADER_SIZE)
    }
    SIZE := 2 * NODE_HEADER_SIZE + ALIGNEDSIZE
  }

  def encode_padding_node(BUF: buffer, ERR: Ref[error]): Unit = {
    val SIZE: Int = BUF.length - 2 * NODE_HEADER_SIZE
    encode_header(types.node_header.nodeheader(SIZE, true), 0, BUF, ERR)
    BUF.copy(validtrailer, 0, BUF.length - NODE_HEADER_SIZE, NODE_HEADER_SIZE)
  }

  override def format(VOLSIZE: Int, WMAXINO0: Int, ERR: Ref[error]): Unit = {
    LPT.allocate(VOLSIZE, types.lprops.uninit)
    LPT.fill(types.lprops.mklp(0, 0, types.lpropflags.LP_FREE, 0))
    val ADR: address = types.address.at(0, 0, 0)
    awbuf.format(VOLSIZE, LPT, ADR, WMAXINO0, ERR)
    if (ERR.get != types.error.ESUCCESS) {
      debug("persistence: wbuf format failed")
    } else {
      LOG.clear
      create_freelist()
      gc_heap_empty(GCHEAP)
    }
  }

  def gc_heap_add_log(): Unit = {
    while (! LOG.isEmpty) {
      val LNUM: Int = LOG.head
      val KI: keyindex = types.keyindex.key_index(LPT(LNUM).ref_size, LNUM)
      gc_heap_insert(KI, GCHEAP, LPT)
      LOG.removeHead
    }
  }

  def gc_heap_empty(bh: binheap): Unit = {
    bh := types.binheap.bin_heap(new key_array(1).fill(types.keyindex.uninit), 0)
  }

  def gc_heap_get_min(bh: binheap, n: Ref[Int]): Unit = {
    if (bh.size == 0)
      assert(false, """abort""")
    
    n := bh.ar(0).idx
  }

  def gc_heap_insert(ki: keyindex, bh: binheap, lpt: lp_array): Unit = {
    if (! (ki.idx < lpt.length))
      assert(false, """abort""")
    
    bin_min_heap_resize(bh.size + 1, bh)
    bh.ar(bh.size) = ki.deepCopy
    lpt(ki.idx).gcheapidx = bh.size
    bh.size = bh.size + 1
    bin_min_heap_bubble_up(bh.size - 1, bh, lpt)
  }

  def gc_heap_is_empty(bh: binheap, empty_ : Ref[Boolean]): Unit = {
    empty_ := (bh.size == 0)
  }

  def gc_heap_remove(n: Int, bh: binheap, lpt: lp_array): Unit = {
    if (! (n < bh.size))
      assert(false, """abort""")
    
    bh.size = bh.size - 1
    if (bh.size != 0 && bh.size != n) {
      bh.ar(n) = bh.ar(bh.size).deepCopy
      lpt(bh.ar(n).idx).gcheapidx = n
      if (n != 0 && bh.ar(n).key < bh.ar((n - 1) / 2).key) {
        bin_min_heap_bubble_up(n, bh, lpt)
      } else {
        bin_min_heap_bubble_down(n, bh, lpt)
      }
    }
  }

  def gc_heap_update(n: Int, natkey: Int, bh: binheap, lpt: lp_array): Unit = {
    if (! (n < bh.size))
      assert(false, """abort""")
    
    val natkey0: Int = bh.ar(n).key
    bh.ar(n).key = natkey
    if (natkey < natkey0) {
      bin_min_heap_bubble_up(n, bh, lpt)
    } else {
      bin_min_heap_bubble_down(n, bh, lpt)
    }
  }

  override def get_block_free_size(N: Ref[Int]): Unit = {
    val WBUFLEB0 = Ref[bufleb](types.bufleb.uninit)
    awbuf.get_buf(WBUFLEB0)
    if (WBUFLEB0.get == types.bufleb.nobuffer) {
      N := 0
    } else {
      N := LEB_SIZE - LPT(WBUFLEB0.get.leb).size
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

  override def is_log_empty(EMPTY_ : Ref[Boolean]): Unit = {
    EMPTY_ := LOG.isEmpty
  }

  override def is_readonly(ROFS: Ref[Boolean]): Unit = {
    awbuf.is_readonly(ROFS)
  }

  override def read_gblock_nodes(LNUM: Int, ADRLIST: address_list, NODELIST: group_node_list, ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(LEB_SIZE).fill(0.toByte)
    awbuf.read_buf(LNUM, 0, LEB_SIZE, BUF, ERR)
    var OFFSET: Int = 0
    while (ERR.get == types.error.ESUCCESS && OFFSET < LEB_SIZE) {
      if (! (OFFSET + NODE_HEADER_SIZE <= BUF.length) || isempty(BUF, OFFSET, NODE_HEADER_SIZE)) {
        ERR := types.error.ENOENT
      } else {
        val NDHD = Ref[node_header](types.node_header.uninit)
        decode_header(OFFSET, BUF, NDHD, ERR)
        if (ERR.get != types.error.ESUCCESS) {
          debug("persistence: read_gblock_nodes could not decode header in LEB " + (toStr(LNUM) + (" at offset " + toStr(OFFSET))))
        } else {
          val SIZE: Int = 2 * NODE_HEADER_SIZE + alignUp(NDHD.get.size, 2 * NODE_HEADER_SIZE)
          if (OFFSET + SIZE > LEB_SIZE) {
            debug("persistence: read_gblock_nodes ignoring node of size " + (toStr(SIZE) + (" at offset " + (toStr(OFFSET) + (" in LEB " + toStr(LNUM))))))
            ERR := types.error.ENOENT
          } else           if (! rangeeq(BUF, OFFSET + (NODE_HEADER_SIZE + alignUp(NDHD.get.size, 2 * NODE_HEADER_SIZE)), validtrailer, 0, NODE_HEADER_SIZE)) {
            ERR := types.error.ENOENT
            debug("persistence: read_gblock_nodes partial node at offset " + (toStr(OFFSET) + (" in LEB " + toStr(LNUM))))
          } else           if (! NDHD.get.ispadding) {
            val GND = Ref[group_node](types.group_node.uninit)
            decode_group_node(OFFSET, SIZE, BUF, GND, ERR)
            if (ERR.get != types.error.ESUCCESS) {
              debug("persistence: read_gblock_nodes could not decode node at offset " + (toStr(OFFSET) + (" in LEB " + toStr(LNUM))))
            } else {
              NODELIST += GND.get
              ADRLIST += types.address.at(LNUM, OFFSET, SIZE)
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
    LOG := LOG0.deepCopy
    if (ERR.get != types.error.ESUCCESS) {
      debug("persistence: wbuf recover failed")
    } else {
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
      LPT(N).size = LEB_SIZE
      LOG0.removeHead
    }
  }

  def replay_lpt(): Unit = {
    var N: Int = 0
    while (N < LPT.length) {
      if (LPT(N).flags != types.lpropflags.LP_FREE && ! LOG.contains(N)) {
        if (LPT(N).flags == types.lpropflags.LP_INDEX_NODES) {
          LPT(N).size = LEB_SIZE
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
      gc_heap_update(LPT(LNUM).gcheapidx, N, GCHEAP, LPT)
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
    if (AROFS0.get != true) {
      if (WBUFLEB0.get.isInstanceOf[types.bufleb.buffered]) {
        val OFFSET: Int = LPT(WBUFLEB0.get.leb).size
        if (! is_aligned(OFFSET, EB_PAGE_SIZE)) {
          val SIZE: Int = EB_PAGE_SIZE - OFFSET % EB_PAGE_SIZE
          val BUF: buffer = new buffer(SIZE).fill(0.toByte)
          encode_padding_node(BUF, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            awbuf.write_buf(BUF.length, BUF, ERR)
            if (ERR.get == types.error.ESUCCESS) {
              LPT(WBUFLEB0.get.leb).size = LPT(WBUFLEB0.get.leb).size + SIZE
            } else {
              debug("persistence: flushing of LEB " + (toStr(WBUFLEB0.get.leb) + " failed"))
              LPT(WBUFLEB0.get.leb).size = LEB_SIZE
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

