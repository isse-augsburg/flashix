// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import types._
import types.error.error
import types.lpropflags.lpropflags

class Persistence(val NL : nat_list, var FORCECOMMIT : Boolean, var SB : superblock, var LOGOFFSET : Int, var GCHEAP : binheap, val LPT : lp_array, val FREELIST : nat_list, val awbuf : AWBUF)(implicit _algebraic_implicit: algebraic.Algebraic) extends AbstractPersistence {
  import _algebraic_implicit._

  private def awbuf_read_gnd_headerless_buf(LNUM: Int, OFFSET: Int, N: Int, GND: Ref[group_node], ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(N).fill(0.toByte)
    awbuf.awbuf_read_buf(LNUM, OFFSET, N, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (is_gndnode(BUF))
        GND := unpack_gnd(BUF)
      else
        ERR := types.error.ENOENT
    }
  }

  private def awbuf_read_ind_headerless_buf(LNUM: Int, OFFSET: Int, N: Int, IND: Ref[index_node], ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(N).fill(0.toByte)
    awbuf.awbuf_read_buf(LNUM, OFFSET, N, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (is_indnode(BUF))
        IND := unpack_ind(BUF)
      else
        ERR := types.error.ENOENT
    }
  }

  private def bin_min_heap_bubble_down(n0: Int, bh: Ref[binheap], lpt: lp_array): Unit = {
    var n1: Int = n0
    while (2 * n1 + 1 < bh.get.size && (bh.get.ar(2 * n1 + 1).key < bh.get.ar(n1).key || 2 * n1 + 2 < bh.get.size && bh.get.ar(2 * n1 + 2).key < bh.get.ar(n1).key)) {
      val ki0: keyindex = bh.get.ar(2 * n1 + 1)
      val ki: keyindex = bh.get.ar(n1)
      if (2 * n1 + 2 < bh.get.size && bh.get.ar(2 * n1 + 2).key < ki0.key) {
        val ki1: keyindex = bh.get.ar(2 * n1 + 2)
        bh.get.ar(n1) = ki1
        lpt(ki1.idx).gcheapidx = n1
        bh.get.ar(2 * n1 + 2) = ki
        lpt(ki.idx).gcheapidx = 2 * n1 + 2
        n1 = 2 * n1 + 2
      } else {
        bh.get.ar(n1) = ki0
        lpt(ki0.idx).gcheapidx = n1
        bh.get.ar(2 * n1 + 1) = ki
        lpt(ki.idx).gcheapidx = 2 * n1 + 1
        n1 = 2 * n1 + 1
      }
    }
  }

  private def bin_min_heap_bubble_up(n0: Int, bh: Ref[binheap], lpt: lp_array): Unit = {
    var n1: Int = n0
    while (n1 != 0 && bh.get.ar(n1).key < bh.get.ar((n1 - 1) / 2).key) {
      val ki: keyindex = bh.get.ar(n1)
      val ki0: keyindex = bh.get.ar((n1 - 1) / 2)
      bh.get.ar(n1) = ki0
      lpt(ki0.idx).gcheapidx = n1
      n1 = (n1 - 1) / 2
      bh.get.ar(n1) = ki
      lpt(ki.idx).gcheapidx = n1
    }
  }

  private def bin_min_heap_next_2pot(n0: Int, n1: Ref[Int]): Unit = {
    while (n1.get < n0) {
      n1 := n1.get * 2
    }
  }

  private def bin_min_heap_resize(n1: Int, bh: Ref[binheap]): Unit = {
    if (n1 > bh.get.size) {
      val n0 = new Ref[Int](bh.get.ar.length)
      bin_min_heap_next_2pot(n1, n0)
      val kar: key_array = new key_array(n0.get).fill(keyindex.uninit)
      kar.copy(bh.get.ar, 0, 0, bh.get.size)
      bh := bh.get.copy(ar = kar).deepCopy
    }
  }

  private def gc_heap_empty(bh: Ref[binheap]): Unit = {
    bh := types.binheap.bin_heap(new key_array(1).fill(keyindex.uninit), 0)
  }

  private def gc_heap_get_min(bh: binheap, n0: Ref[Int]): Unit = {
    if (bh.size == 0)
      assert(false)
    
    n0 := bh.ar(0).idx
  }

  private def gc_heap_insert(ki: keyindex, bh: Ref[binheap], lpt: lp_array): Unit = {
    if (! (ki.idx < lpt.length))
      assert(false)
    
    bin_min_heap_resize(bh.get.size + 1, bh)
    bh.get.ar(bh.get.size) = ki
    lpt(ki.idx).gcheapidx = bh.get.size
    bh := bh.get.copy(size = bh.get.size + 1).deepCopy
    bin_min_heap_bubble_up(bh.get.size - 1, bh, lpt)
  }

  private def gc_heap_is_empty(bh: binheap, empty_ : Ref[Boolean]): Unit = {
    empty_ := (bh.size == 0)
  }

  private def gc_heap_remove(n0: Int, bh: Ref[binheap], lpt: lp_array): Unit = {
    if (! (n0 < bh.get.size))
      assert(false)
    
    bh := bh.get.copy(size = bh.get.size - 1).deepCopy
    if (bh.get.size != 0 && bh.get.size != n0) {
      bh.get.ar(n0) = bh.get.ar(bh.get.size)
      lpt(bh.get.ar(n0).idx).gcheapidx = n0
      if (n0 != 0 && bh.get.ar(n0).key < bh.get.ar((n0 - 1) / 2).key) {
        bin_min_heap_bubble_up(n0, bh, lpt)
      } else {
        bin_min_heap_bubble_down(n0, bh, lpt)
      }
    }
  }

  private def gc_heap_update(n2: Int, n0: Int, bh: Ref[binheap], lpt: lp_array): Unit = {
    if (! (n2 < bh.get.size))
      assert(false)
    
    val n1: Int = bh.get.ar(n2).key
    bh.get.ar(n2) = bh.get.ar(n2).copy(key = n0)
    if (n0 < n1) {
      bin_min_heap_bubble_up(n2, bh, lpt)
    } else {
      bin_min_heap_bubble_down(n2, bh, lpt)
    }
  }

  private def node_header_read(LNUM: Int, OFFSET: Int, N: Int, NDHD: Ref[node_header], ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(N).fill(0.toByte)
    awbuf.awbuf_read_buf(LNUM, OFFSET, N, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (is_node_header(BUF))
        NDHD := unpack_node_header(BUF)
      else
        ERR := types.error.ENOENT
    }
  }

  private def node_header_write(LNUM: Int, NDHD: node_header, N: Ref[Int], ERR: Ref[error]): Unit = {
    val BUF: buffer = pack_node_header(NDHD)
    N := BUF.length
    awbuf.awbuf_write_buf(LNUM, N.get, BUF, ERR)
  }

  override def persistence_add_gnd(LNUM: Int, GND: group_node, ADR: Ref[address], ERR: Ref[error]): Unit = {
    if (LPT(LNUM).size + flashsize(GND) > LEB_SIZE)
      ERR := types.error.ENOSPC
    else {
      ubifs_ensure_buffered(LNUM)
      val SIZE = new Ref[Int](0)
      ubifs_persistence_write_gnd(LNUM, GND, SIZE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        ADR := types.address.at(LNUM, LPT(LNUM).size, SIZE.get)
        LPT(LNUM).size = LPT(LNUM).size + SIZE.get
      } else
        LPT(LNUM).size = LEB_SIZE
    }
  }

  override def persistence_add_ind(LNUM: Int, IND: index_node, ADR: Ref[address], ERR: Ref[error]): Unit = {
    if (LPT(LNUM).size + flashsize(IND) > LEB_SIZE)
      ERR := types.error.ENOSPC
    else {
      ubifs_ensure_buffered(LNUM)
      val SIZE = new Ref[Int](0)
      ubifs_persistence_write_ind(LNUM, IND, SIZE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        ADR := types.address.at(LNUM, LPT(LNUM).size, SIZE.get)
        LPT(LNUM).size = LPT(LNUM).size + SIZE.get
      } else
        LPT(LNUM).size = LEB_SIZE
    }
  }

  override def persistence_allocate_gnd(N: Ref[Int], ERR: Ref[error]): Unit = {
    ubifs_allocate_leb(N, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      ubifs_add_bud(N.get, ERR)
      if (ERR.get != types.error.ESUCCESS)
        N.get +=: FREELIST
      
    }
    if (ERR.get == types.error.ESUCCESS) {
      LPT(N.get).flags = types.lpropflags.LP_GROUP_NODES
      NL += N.get
    }
  }

  override def persistence_allocate_ind(N: Ref[Int], ERR: Ref[error]): Unit = {
    ubifs_allocate_leb(N, ERR)
    if (ERR.get == types.error.ESUCCESS)
      LPT(N.get).flags = types.lpropflags.LP_INDEX_NODES
    
  }

  override def persistence_deallocate_gnd(N: Int, ERR: Ref[error]): Unit = {
    if (SB.main <= N && N <= LPT.length) {
      awbuf.awbuf_unmap(N, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        LPT(N).flags = types.lpropflags.LP_FREE
        LPT(N).ref_size = 0
        FREELIST += N
        val GCHEAP0: Ref[binheap] = new Ref[binheap](GCHEAP)
        gc_heap_remove(LPT(N).gcheapidx, GCHEAP0, LPT)
        GCHEAP = GCHEAP0.get
      }
    } else
      ERR := types.error.EINVAL
  }

  override def persistence_flush_gnd(LNUM: Int, ERR: Ref[error]): Unit = {
    ubifs_flush(LNUM, ERR)
  }

  override def persistence_flush_ind(LNUM: Int, ERR: Ref[error]): Unit = {
    ubifs_flush(LNUM, ERR)
  }

  override def persistence_format(VOLSIZE: Int, ERR: Ref[error]): Unit = {
    awbuf.awbuf_format(VOLSIZE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val N = new Ref[Int](0)
      awbuf.awbuf_get_volume_size(N)
      if (N.get <= MAINLNUM)
        ERR := types.error.ENOSPC
      else {
        LPT.allocate(N.get)
        LPT.fill(types.lprops.mklp(0, 0, types.lpropflags.LP_FREE, 0))
        LPT(MAINLNUM).flags = types.lpropflags.LP_INDEX_NODES
        val IND: index_node = initial_ind
        if (flashsize(IND) >= LEB_SIZE)
          ERR := types.error.ENOSPC
        
        if (ERR.get == types.error.ESUCCESS) {
          awbuf.awbuf_remap(MAINLNUM, ERR)
        }
        val ADR = new Ref[address](address.uninit)
        if (ERR.get == types.error.ESUCCESS) {
          persistence_add_ind(MAINLNUM, IND, ADR, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          ubifs_flush(MAINLNUM, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          LPT(MAINLNUM).ref_size = ADR.get.size
          SB = types.superblock.mksb(ADR.get, 1, ORPHLNUM, LPTLNUM, LOGLNUM, MAINLNUM)
          NL.clear
          LOGOFFSET = 0
          FORCECOMMIT = false
          awbuf.awbuf_remap(ORPHLNUM, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          awbuf.awbuf_remap(LPTLNUM, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          awbuf.awbuf_remap(LOGLNUM, ERR)
        }
        val SIZE = new Ref[Int](0)
        if (ERR.get == types.error.ESUCCESS) {
          ubifs_persistence_write_lpt(LPTLNUM, 0, SIZE, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          ubifs_persistence_write_orphans(ORPHLNUM, 0, new key_set(), SIZE, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          ubifs_persistence_change_sb(SBLNUM, SB, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          ubifs_persistence_create_freelist()
        }
        if (ERR.get == types.error.ESUCCESS) {
          val GCHEAP0: Ref[binheap] = new Ref[binheap](GCHEAP)
          gc_heap_empty(GCHEAP0)
          GCHEAP = GCHEAP0.get
        }
      }
    }
  }

  override def persistence_get_gblock_refsize(LNUM: Int, N: Ref[Int]): Unit = {
    if (LNUM < LPT.length)
      N := LPT(LNUM).ref_size
    
  }

  override def persistence_get_gblock_size(LNUM: Int, N: Ref[Int]): Unit = {
    if (LNUM < LPT.length)
      N := LPT(LNUM).size
    
  }

  override def persistence_get_gc_block(N: Ref[Int], ERR: Ref[error]): Unit = {
    val empty_ = new Ref[Boolean](helpers.scala.Boolean.uninit)
    gc_heap_is_empty(GCHEAP, empty_)
    if (empty_.get)
      ERR := types.error.ENOSPC
    else {
      gc_heap_get_min(GCHEAP, N)
      ERR := types.error.ESUCCESS
    }
  }

  override def persistence_get_iblock_refsize(LNUM: Int, N: Ref[Int]): Unit = {
    if (LNUM < LPT.length)
      N := LPT(LNUM).ref_size
    
  }

  override def persistence_get_iblock_size(LNUM: Int, N: Ref[Int]): Unit = {
    if (LNUM < LPT.length)
      N := LPT(LNUM).size
    
  }

  override def persistence_get_maxino(MAXINO: Ref[Int]): Unit = {
    MAXINO := SB.maxino
  }

  override def persistence_get_root(ROOTADR: Ref[address]): Unit = {
    ROOTADR := SB.indexaddr
  }

  override def persistence_read_gblock_nodes(LNUM: Int, ADRLIST: address_list, GNDLIST: group_node_list, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    var OFFSET: Int = 0
    while (ERR.get == types.error.ESUCCESS && OFFSET < LEB_SIZE) {
      val SIZE = new Ref[Int](0)
      val BOOLVAR = new Ref[Boolean](helpers.scala.Boolean.uninit)
      val GND = new Ref[group_node](group_node.uninit)
      ubifs_persistence_read_gnd(LNUM, OFFSET, GND, BOOLVAR, SIZE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (BOOLVAR.get != true) {
          ADRLIST += types.address.at(LNUM, OFFSET, SIZE.get)
          GNDLIST += GND.get
        }
        OFFSET = OFFSET + SIZE.get
      }
    }
    if (ERR.get == types.error.ENOENT)
      ERR := types.error.ESUCCESS
    
  }

  override def persistence_read_gnd(ADR: address, GND: Ref[group_node], ERR: Ref[error]): Unit = {
    val SIZE = new Ref[Int](0)
    val BOOLVAR = new Ref[Boolean](helpers.scala.Boolean.uninit)
    ubifs_persistence_read_gnd(ADR.lnum, ADR.pos, GND, BOOLVAR, SIZE, ERR)
  }

  override def persistence_read_ind(ADR: address, IND: Ref[index_node], ERR: Ref[error]): Unit = {
    val SIZE = new Ref[Int](0)
    val BOOLVAR = new Ref[Boolean](helpers.scala.Boolean.uninit)
    ubifs_persistence_read_ind(ADR.lnum, ADR.pos, IND, BOOLVAR, SIZE, ERR)
  }

  override def persistence_read_log(NL0: nat_list): Unit = {
    NL0 := NL.deepCopy
  }

  override def persistence_read_orph(KS: key_set, ERR: Ref[error]): Unit = {
    ubifs_persistence_read_orphans(SB.orph, 0, LEB_SIZE, KS, ERR)
  }

  override def persistence_requires_commit(COMMIT_ : Ref[Boolean]): Unit = {
    COMMIT_ := (FORCECOMMIT || LOGOFFSET == LEB_SIZE)
  }

  override def persistence_set_gblock_refsize(LNUM: Int, N: Int): Unit = {
    if (N < 0) {
      throw new UnsupportedOperationException("refsize of group block " + LNUM + " is negative " + N)
    }
    if (LNUM < LPT.length) {
      LPT(LNUM).ref_size = N
      if (! NL.contains(LNUM)) {
        val GCHEAP0: Ref[binheap] = new Ref[binheap](GCHEAP)
        gc_heap_update(LPT(LNUM).gcheapidx, N, GCHEAP0, LPT)
        GCHEAP = GCHEAP0.get
      }
    }
  }

  override def persistence_set_iblock_refsize(LNUM: Int, N: Int): Unit = {
    if (N < 0) {
      throw new UnsupportedOperationException("refsize of index block " + LNUM + " is negative " + N)
    }
    if (LNUM < LPT.length)
      LPT(LNUM).ref_size = N
    
  }

  override def persistence_write_superblock(ROOTADR: address, MAXINO: Int, KS: key_set, ERR: Ref[error]): Unit = {
    ubifs_cleanup_bufs()
    var SB0: superblock = other_sb(SB)
    awbuf.awbuf_remap(SB0.lpt, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val SIZE = new Ref[Int](0)
      if (ERR.get == types.error.ESUCCESS) {
        ubifs_persistence_write_lpt(SB0.lpt, 0, SIZE, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        awbuf.awbuf_remap(SB0.orph, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        ubifs_persistence_write_orphans(SB0.orph, 0, KS, SIZE, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        awbuf.awbuf_remap(SB0.log, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        SB0 = SB0.copy(indexaddr = ROOTADR)
        SB0 = SB0.copy(maxino = MAXINO)
        ubifs_persistence_change_sb(SBLNUM, SB0, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        LOGOFFSET = 0
        SB = SB0
        FORCECOMMIT = false
        ubifs_gc_heap_add_log()
        ubifs_cleanup_lpt()
      }
    }
  }

  private def ubifs_add_bud(N0: Int, ERR: Ref[error]): Unit = {
    var N = N0
    if (FORCECOMMIT != true) {
      if (LOGOFFSET + EB_PAGE_SIZE <= LEB_SIZE) {
        val RND: ref_node = types.ref_node.rnode(N)
        val N0: Ref[Int] = new Ref[Int](N)
        ubifs_persistence_write_rnd(SB.log, LOGOFFSET, RND, N0, ERR)
        N = N0.get
        if (ERR.get != types.error.ESUCCESS) {
          FORCECOMMIT = true
          ERR := types.error.ECOMMIT
        } else
          LOGOFFSET = LOGOFFSET + EB_PAGE_SIZE
      } else {
        FORCECOMMIT = true
        ERR := types.error.ECOMMIT
      }
    } else
      ERR := types.error.ECOMMIT
  }

  private def ubifs_allocate_leb(N: Ref[Int], ERR: Ref[error]): Unit = {
    if (FREELIST.isEmpty)
      ERR := types.error.ENOSPC
    else {
      N := FREELIST.head
      awbuf.awbuf_remap(N.get, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        FREELIST.removeHead
        LPT(N.get).size = 0
        LPT(N.get).ref_size = 0
      }
    }
  }

  private def ubifs_cleanup_bufs(): Unit = {
    val WBS0: nat_set = new nat_set()
    awbuf.awbuf_get_bufs(WBS0)
    while (! WBS0.isEmpty) {
      ChooseIn((WBS0).set, (LNUM : Int) =>
      {
        WBS0 -= LNUM
        LPT(LNUM).size = LEB_SIZE
      })
    }
    awbuf.awbuf_destroy_bufs()
  }

  private def ubifs_cleanup_lpt(): Unit = {
    var N: Int = 0
    while (N < LPT.length) {
      if (LPT(N).ref_size == 0 && LPT(N).flags != types.lpropflags.LP_FREE) {
        if (LPT(N).flags == types.lpropflags.LP_GROUP_NODES) {
          val GCHEAP0: Ref[binheap] = new Ref[binheap](GCHEAP)
          gc_heap_remove(LPT(N).gcheapidx, GCHEAP0, LPT)
          GCHEAP = GCHEAP0.get
        }
        LPT(N).flags = types.lpropflags.LP_FREE
        LPT(N).size = 0
        FREELIST += N
        val ERR = new Ref[error](error.uninit)
        awbuf.awbuf_unmap(N, ERR)
      }
      N = N + 1
    }
  }

  private def ubifs_ensure_buffered(LNUM: Int): Unit = {
    val ISBUF = new Ref[Boolean](helpers.scala.Boolean.uninit)
    awbuf.awbuf_is_buffered(LNUM, ISBUF)
    if (ISBUF.get != true) {
      awbuf.awbuf_create_buf(LNUM, LPT(LNUM).size)
    }
  }

  private def ubifs_flush(LNUM: Int, ERR: Ref[error]): Unit = {
    val OFFSET: Int = LPT(LNUM).size
    if (OFFSET == LEB_SIZE)
      ERR := types.error.ESUCCESS
    else {
      val ISBUF = new Ref[Boolean](helpers.scala.Boolean.uninit)
      awbuf.awbuf_is_buffered(LNUM, ISBUF)
      if (ISBUF.get) {
        if (is_aligned(OFFSET, EB_PAGE_SIZE))
          ERR := types.error.ESUCCESS
        else {
          val SIZE: Int = EB_PAGE_SIZE - (OFFSET % EB_PAGE_SIZE + 2 * NODE_HEADER_SIZE)
          val N0 = new Ref[Int](0)
          node_header_write(LNUM, types.node_header.nodeheader(SIZE, true), N0, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            val BUF: buffer = mkzbuf(SIZE).deepCopy
            awbuf.awbuf_write_buf(LNUM, BUF.length, BUF, ERR)
          }
          if (ERR.get == types.error.ESUCCESS) {
            awbuf.awbuf_write_buf(LNUM, NODE_HEADER_SIZE, validtrailer, ERR)
          }
          if (ERR.get == types.error.ESUCCESS)
            LPT(LNUM).size = LPT(LNUM).size + (SIZE + 2 * NODE_HEADER_SIZE)
          else
            LPT(LNUM).size = LEB_SIZE
        }
      } else
        ERR := types.error.ESUCCESS
    }
  }

  private def ubifs_gc_heap_add_log(): Unit = {
    while (! NL.isEmpty) {
      val LNUM: Int = NL.head
      val GCHEAP0: Ref[binheap] = new Ref[binheap](GCHEAP)
      gc_heap_insert(types.keyindex.key_index(LPT(LNUM).ref_size, LNUM), GCHEAP0, LPT)
      GCHEAP = GCHEAP0.get
      NL.removeHead
    }
  }

  private def ubifs_persistence_change_sb(LNUM: Int, SB: superblock, ERR: Ref[error]): Unit = {
    val BUF: buffer = pack_sb(SB)
    awbuf.awbuf_change(LNUM, BUF.length, BUF, ERR)
  }

  private def ubifs_persistence_create_freelist(): Unit = {
    FREELIST.clear
    var N: Int = SB.main
    while (N < LPT.length) {
      if (LPT(N).flags == types.lpropflags.LP_FREE)
        FREELIST += N
      
      N = N + 1
    }
  }

  private def ubifs_persistence_create_gcheap(): Unit = {
    val GCHEAP0: Ref[binheap] = new Ref[binheap](GCHEAP)
    gc_heap_empty(GCHEAP0)
    GCHEAP = GCHEAP0.get
    var N: Int = 0
    while (N < LPT.length) {
      if (LPT(N).flags == types.lpropflags.LP_GROUP_NODES && ! NL.contains(N)) {
        val GCHEAP0: Ref[binheap] = new Ref[binheap](GCHEAP)
        gc_heap_insert(types.keyindex.key_index(LPT(N).ref_size, N), GCHEAP0, LPT)
        GCHEAP = GCHEAP0.get
      }
      N = N + 1
    }
  }

  private def ubifs_persistence_read_gnd(LNUM: Int, OFFSET: Int, GND: Ref[group_node], ISPAD: Ref[Boolean], SIZE: Ref[Int], ERR: Ref[error]): Unit = {
    if (OFFSET + NODE_HEADER_SIZE > LEB_SIZE)
      ERR := types.error.ENOENT
    else {
      val NDHD = new Ref[node_header](node_header.uninit)
      node_header_read(LNUM, OFFSET, NODE_HEADER_SIZE, NDHD, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        ISPAD := NDHD.get.ispadding
        if (OFFSET + (2 * NODE_HEADER_SIZE + NDHD.get.size) <= LEB_SIZE) {
          val TRAILER: buffer = new buffer(NODE_HEADER_SIZE).fill(0.toByte)
          awbuf.awbuf_read_buf(LNUM, OFFSET + (NODE_HEADER_SIZE + NDHD.get.size), NODE_HEADER_SIZE, TRAILER, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            if (isvalidtrailer(TRAILER)) {
              if (ISPAD.get != true) {
                awbuf_read_gnd_headerless_buf(LNUM, OFFSET + NODE_HEADER_SIZE, NDHD.get.size, GND, ERR)
              }
              SIZE := 2 * NODE_HEADER_SIZE + NDHD.get.size
            } else
              ERR := types.error.ENOENT
          }
        } else
          ERR := types.error.ENOENT
      }
    }
  }

  private def ubifs_persistence_read_ind(LNUM: Int, OFFSET: Int, IND: Ref[index_node], ISPAD: Ref[Boolean], SIZE: Ref[Int], ERR: Ref[error]): Unit = {
    if (OFFSET + NODE_HEADER_SIZE > LEB_SIZE)
      ERR := types.error.ENOENT
    else {
      val NDHD = new Ref[node_header](node_header.uninit)
      node_header_read(LNUM, OFFSET, NODE_HEADER_SIZE, NDHD, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        ISPAD := NDHD.get.ispadding
        if (OFFSET + (2 * NODE_HEADER_SIZE + NDHD.get.size) <= LEB_SIZE) {
          val TRAILER: buffer = new buffer(NODE_HEADER_SIZE).fill(0.toByte)
          awbuf.awbuf_read_buf(LNUM, OFFSET + (NODE_HEADER_SIZE + NDHD.get.size), NODE_HEADER_SIZE, TRAILER, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            if (isvalidtrailer(TRAILER)) {
              if (ISPAD.get != true) {
                awbuf_read_ind_headerless_buf(LNUM, OFFSET + NODE_HEADER_SIZE, NDHD.get.size, IND, ERR)
              }
              SIZE := 2 * NODE_HEADER_SIZE + NDHD.get.size
            } else
              ERR := types.error.ENOENT
          }
        } else
          ERR := types.error.ENOENT
      }
    }
  }

  private def ubifs_persistence_read_lpt(LNUM: Int, OFFSET: Int, N: Int, ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(N).fill(0.toByte)
    awbuf.awbuf_read(LNUM, OFFSET, 0, N, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (is_lpt(BUF))
        LPT := unpack_lpt(BUF)
      else
        ERR := types.error.ENOENT
    }
  }

  private def ubifs_persistence_read_orphans(LNUM: Int, OFFSET: Int, N: Int, KS: key_set, ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(N).fill(0.toByte)
    awbuf.awbuf_read(LNUM, OFFSET, 0, N, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (is_orphans(BUF))
        KS := unpack_orphans(BUF)
      else
        ERR := types.error.ENOENT
    }
  }

  private def ubifs_persistence_read_rnd(LNUM: Int, OFFSET: Int, N: Int, RND: Ref[ref_node], ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(N).fill(0.toByte)
    awbuf.awbuf_read(LNUM, OFFSET, 0, N, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (is_refnode(BUF))
        RND := unpack_rnd(BUF)
      else
        ERR := types.error.ENOENT
    }
  }

  private def ubifs_persistence_read_sb(LNUM: Int, OFFSET: Int, N: Int, ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(N).fill(0.toByte)
    awbuf.awbuf_read(LNUM, OFFSET, 0, N, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (is_sb(BUF))
        SB = unpack_sb(BUF)
      else
        ERR := types.error.ENOENT
    }
  }

  def ubifs_persistence_recover(ERR: Ref[error]): Unit = {
    LOGOFFSET = 0
    FORCECOMMIT = true
    NL.clear
    ubifs_persistence_read_sb(SBLNUM, 0, LEB_SIZE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      ubifs_persistence_read_lpt(SB.lpt, 0, LEB_SIZE, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      ubifs_read_flash_log(ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val NL0: nat_list = NL.deepCopy
        ubifs_persistence_replay_log(NL0)
        ubifs_persistence_replay_lpt()
        ubifs_persistence_create_freelist()
        ubifs_persistence_create_gcheap()
      }
    }
  }

  private def ubifs_persistence_replay_log(NL: nat_list): Unit = {
    while (! NL.isEmpty) {
      val N: Int = NL.head
      LPT(N).flags = types.lpropflags.LP_GROUP_NODES
      LPT(N).size = LEB_SIZE
      NL.removeHead
    }
  }

  private def ubifs_persistence_replay_lpt(): Unit = {
    var N: Int = 0
    while (N < LPT.length) {
      if (LPT(N).flags != types.lpropflags.LP_FREE && ! NL.contains(N)) {
        if (LPT(N).flags == types.lpropflags.LP_INDEX_NODES)
          LPT(N).size = LEB_SIZE
        
        if (LPT(N).ref_size == 0) {
          LPT(N).flags = types.lpropflags.LP_FREE
          LPT(N).size = 0
          val ERR = new Ref[error](error.uninit)
          awbuf.awbuf_unmap(N, ERR)
        }
      }
      N = N + 1
    }
  }

  private def ubifs_persistence_write_gnd(LNUM: Int, GND: group_node, SIZE: Ref[Int], ERR: Ref[error]): Unit = {
    val BUF: buffer = pack_gnd(GND)
    node_header_write(LNUM, types.node_header.nodeheader(BUF.length, false), SIZE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      awbuf.awbuf_write_buf(LNUM, BUF.length, BUF, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      awbuf.awbuf_write_buf(LNUM, NODE_HEADER_SIZE, validtrailer, ERR)
    }
    if (ERR.get == types.error.ESUCCESS)
      SIZE := 2 * NODE_HEADER_SIZE + BUF.length
    
  }

  private def ubifs_persistence_write_ind(LNUM: Int, IND: index_node, SIZE: Ref[Int], ERR: Ref[error]): Unit = {
    val BUF: buffer = pack_ind(IND)
    node_header_write(LNUM, types.node_header.nodeheader(BUF.length, false), SIZE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      awbuf.awbuf_write_buf(LNUM, BUF.length, BUF, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      awbuf.awbuf_write_buf(LNUM, NODE_HEADER_SIZE, validtrailer, ERR)
    }
    if (ERR.get == types.error.ESUCCESS)
      SIZE := 2 * NODE_HEADER_SIZE + BUF.length
    
  }

  private def ubifs_persistence_write_lpt(LNUM: Int, OFFSET: Int, N: Ref[Int], ERR: Ref[error]): Unit = {
    val BUF: buffer = pack_lpt(LPT)
    N := BUF.length
    awbuf.awbuf_write(LNUM, OFFSET, 0, BUF.length, BUF, ERR)
  }

  private def ubifs_persistence_write_orphans(LNUM: Int, OFFSET: Int, KS: key_set, N: Ref[Int], ERR: Ref[error]): Unit = {
    val BUF: buffer = pack_orphans(KS)
    N := BUF.length
    awbuf.awbuf_write(LNUM, OFFSET, 0, BUF.length, BUF, ERR)
  }

  private def ubifs_persistence_write_rnd(LNUM: Int, OFFSET: Int, RND: ref_node, N: Ref[Int], ERR: Ref[error]): Unit = {
    val BUF: buffer = pack_rnd(RND)
    N := BUF.length
    awbuf.awbuf_write(LNUM, OFFSET, 0, BUF.length, BUF, ERR)
  }

  private def ubifs_read_flash_log(ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    var OFFSET: Int = 0
    while (ERR.get == types.error.ESUCCESS && OFFSET < LEB_SIZE) {
      val RND = new Ref[ref_node](ref_node.uninit)
      ubifs_persistence_read_rnd(SB.log, OFFSET, EB_PAGE_SIZE, RND, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        OFFSET = OFFSET + EB_PAGE_SIZE
        NL += RND.get.lnum
      }
    }
    if (ERR.get == types.error.ENOENT)
      ERR := types.error.ESUCCESS
    
  }

}

object Persistence {
  def apply(NL: nat_list, FORCECOMMIT: Boolean, SB: superblock, LOGOFFSET: Int, GCHEAP: binheap, LPT: lp_array, FREELIST: nat_list, awbuf: AWBUF)(implicit _algebraic_implicit: algebraic.Algebraic) = {
    new Persistence(NL, FORCECOMMIT, SB, LOGOFFSET, GCHEAP, LPT, FREELIST, awbuf)
  }
}
