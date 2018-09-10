// Flashix: a verified file system for flash memory
// (c) 2015-2018 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package proc

import encoding.echeader._
import encoding.group_node._
import encoding.index_node._
import encoding.lprops._
import encoding.node_header._
import encoding.ref_node._
import encoding.superblock._
import encoding.vidheader._
import encoding.volid._
import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._
import types._
import types.error.error

trait Procedures {
  def bin_min_heap_bubble_down(n: Int, bh: binheap, ar: gc_array)  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    var m: Int = n
    while (2 * m + 1 < bh.size && (bh.ar(2 * m + 1).key < bh.ar(m).key || 2 * m + 2 < bh.size && bh.ar(2 * m + 2).key < bh.ar(m).key)) {
      val ki: keyindex = bh.ar(m).deepCopy
      val ki0: keyindex = bh.ar(2 * m + 1).deepCopy
      if (2 * m + 2 < bh.size && bh.ar(2 * m + 2).key < ki0.key) {
        val ki1: keyindex = bh.ar(2 * m + 2).deepCopy
        bh.ar(m) = ki1.deepCopy
        ar(ki1.idx) = m
        bh.ar(2 * m + 2) = ki.deepCopy
        ar(ki.idx) = 2 * m + 2
        m = 2 * m + 2
      } else {
        bh.ar(m) = ki0.deepCopy
        ar(ki0.idx) = m
        bh.ar(2 * m + 1) = ki.deepCopy
        ar(ki.idx) = 2 * m + 1
        m = 2 * m + 1
      }
    }
  }
  def bin_min_heap_bubble_up(n: Int, bh: binheap, ar: gc_array)  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    var m: Int = n
    while (m != 0 && bh.ar(m).key < bh.ar((m - 1) / 2).key) {
      val ki: keyindex = bh.ar(m).deepCopy
      val ki0: keyindex = bh.ar((m - 1) / 2).deepCopy
      bh.ar(m) = ki0.deepCopy
      ar(ki0.idx) = m
      m = (m - 1) / 2
      bh.ar(m) = ki.deepCopy
      ar(ki.idx) = m
    }
  }
  def bin_min_heap_next_2pot(minsize: Int, n: Ref[Int])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    while (n.get < minsize) {
      n := n.get * 2
    }
  }
  def bin_min_heap_resize(minsize: Int, bh: binheap)  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    if (minsize > bh.size) {
      val n = Ref[Int](bh.ar.length)
      bin_min_heap_next_2pot(minsize, n)
      val kar: key_array = new key_array(n.get).fill(types.keyindex.uninit)
      kar.copy(bh.ar, 0, 0, bh.size)
      bh.ar = kar
    }
  }
  def cache_transfer_inode_changes(INODE0: inode, INODE: inode)  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    val SIZE: Int = if (INODE.directory) INODE0.size else INODE.size
    INODE := types.inode.mkinode(INODE.ino, INODE.meta, INODE.directory, INODE0.nlink, INODE0.nsubdirs, SIZE)
  }
  def datasize(Buf: buffer, N: Ref[Int])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    datasize_h(Buf, Buf.length, N)
  }
  def datasize_h(Buf: buffer, N1: Int, N0: Ref[Int])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    var N: Int = N1
    while (N != 0 && Buf(N - 1) == empty) {
      N = N - 1
    }
    N0 := N
  }
  def debug(string_variable0: String)  (implicit _algebraic_implicit: algebraic.Algebraic)
  def decode_echeader(n: Int, buf: buffer, a: Ref[echeader], empty_ : Ref[Boolean], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    decode_echeader_nonempty(0, buf, a, empty_, err)
    if (err.get == types.error.ESUCCESS && empty_.get) {
      isempty_h(buf, ENCODED_EC_HEADER_SIZE, n - ENCODED_EC_HEADER_SIZE, empty_)
      if (empty_.get != true) {
        err := types.error.EINVAL
      }
    }
  }
  def decode_echeader_nonempty(n: Int, buf: buffer, a: Ref[echeader], empty_ : Ref[Boolean], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    isempty_h(buf, n, ENCODED_EC_HEADER_SIZE, empty_)
    if (empty_.get) {
      err := types.error.ESUCCESS
    } else {
      val m = Ref[Int](0)
      decode_echeader_empty(n, buf, a, m, err)
    }
  }
  def decode_group_node(OFFSET: Int, SIZE1: Int, BUF: buffer, ND: Ref[group_node], ERR: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    val SIZE = Ref[Int](SIZE1)
    if (SIZE.get < 2 * NODE_HEADER_SIZE) {
      debug("persistence: decode_node expected size wrong")
      ERR := types.error.EINVAL
    } else     if (! (OFFSET + SIZE.get <= BUF.length)) {
      debug("persistence: decode_node buffer insufficient")
      ERR := types.error.EINVAL
    } else {
      val EMPTY_ = Ref[Boolean](helpers.scala.Boolean.uninit)
      val NDHD = Ref[node_header](types.node_header.uninit)
      decode_header(OFFSET, BUF, NDHD, EMPTY_, ERR)
      if (ERR.get == types.error.ESUCCESS && EMPTY_.get != true) {
        if (ERR.get == types.error.ESUCCESS && NDHD.get.ispadding) {
          debug("persistence: decode_node got padding node")
          ERR := types.error.EIO
        }
        if (ERR.get == types.error.ESUCCESS && alignUp(NDHD.get.size, 2 * NODE_HEADER_SIZE) + 2 * NODE_HEADER_SIZE != SIZE.get) {
          debug("persistence: decode_node node of wrong size")
          ERR := types.error.EIO
        }
        if (ERR.get == types.error.ESUCCESS) {
          decode_group_node_headerless(OFFSET + NODE_HEADER_SIZE, BUF, ND, SIZE, ERR)
          if (ERR.get != types.error.ESUCCESS) {
            debug("persistence: decode_node decoding failed")
          } else           if (SIZE.get != NDHD.get.size) {
            debug("persistence: decode_node size of node unexpected")
            ERR := types.error.EINVAL
          }
        }
        if (ERR.get == types.error.ESUCCESS) {
          val EQUALS_ = Ref[Boolean](helpers.scala.Boolean.uninit)
          rangeeq(BUF, OFFSET + (NODE_HEADER_SIZE + alignUp(NDHD.get.size, 2 * NODE_HEADER_SIZE)), validtrailer, 0, NODE_HEADER_SIZE, EQUALS_)
          if (EQUALS_.get != true) {
            debug("persistence: decode_node no validtrailer")
            ERR := types.error.EIO
          }
        }
      } else       if (ERR.get == types.error.ESUCCESS && EMPTY_.get) {
        ERR := types.error.EINVAL
      }
    }
  }
  def decode_header(n: Int, buf: buffer, a: Ref[node_header], empty_ : Ref[Boolean], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    isempty_h(buf, n, NODE_HEADER_SIZE, empty_)
    if (empty_.get) {
      err := types.error.ESUCCESS
    } else {
      val m = Ref[Int](0)
      decode_header_empty(n, buf, a, m, err)
    }
  }
  def decode_index_node(OFFSET: Int, SIZE1: Int, BUF: buffer, ND: Ref[index_node], ERR: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    val SIZE = Ref[Int](SIZE1)
    if (SIZE.get < 2 * NODE_HEADER_SIZE) {
      debug("persistence: decode_node expected size wrong")
      ERR := types.error.EINVAL
    } else     if (! (OFFSET + SIZE.get <= BUF.length)) {
      debug("persistence: decode_node buffer insufficient")
      ERR := types.error.EINVAL
    } else {
      val EMPTY_ = Ref[Boolean](helpers.scala.Boolean.uninit)
      val NDHD = Ref[node_header](types.node_header.uninit)
      decode_header(OFFSET, BUF, NDHD, EMPTY_, ERR)
      if (ERR.get == types.error.ESUCCESS && EMPTY_.get != true) {
        if (ERR.get == types.error.ESUCCESS && NDHD.get.ispadding) {
          debug("persistence: decode_node got padding node")
          ERR := types.error.EIO
        }
        if (ERR.get == types.error.ESUCCESS && alignUp(NDHD.get.size, 2 * NODE_HEADER_SIZE) + 2 * NODE_HEADER_SIZE != SIZE.get) {
          debug("persistence: decode_node node of wrong size")
          ERR := types.error.EIO
        }
        if (ERR.get == types.error.ESUCCESS) {
          decode_index_node_headerless(OFFSET + NODE_HEADER_SIZE, BUF, ND, SIZE, ERR)
          if (ERR.get != types.error.ESUCCESS) {
            debug("persistence: decode_node decoding failed")
          } else           if (SIZE.get != NDHD.get.size) {
            debug("persistence: decode_node size of node unexpected")
            ERR := types.error.EINVAL
          }
        }
        if (ERR.get == types.error.ESUCCESS) {
          val EQUALS_ = Ref[Boolean](helpers.scala.Boolean.uninit)
          rangeeq(BUF, OFFSET + (NODE_HEADER_SIZE + alignUp(NDHD.get.size, 2 * NODE_HEADER_SIZE)), validtrailer, 0, NODE_HEADER_SIZE, EQUALS_)
          if (EQUALS_.get != true) {
            debug("persistence: decode_node no validtrailer")
            ERR := types.error.EIO
          }
        }
      } else       if (ERR.get == types.error.ESUCCESS && EMPTY_.get) {
        ERR := types.error.EINVAL
      }
    }
  }
  def decode_ref_node(n: Int, buf: buffer, a: Ref[ref_node], empty_ : Ref[Boolean], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    decode_ref_node_nonempty(0, buf, a, empty_, err)
    if (err.get == types.error.ESUCCESS && empty_.get) {
      isempty_h(buf, ENCODED_REF_NODE_SIZE, n - ENCODED_REF_NODE_SIZE, empty_)
      if (empty_.get != true) {
        err := types.error.EINVAL
      }
    }
  }
  def decode_ref_node_nonempty(n: Int, buf: buffer, a: Ref[ref_node], empty_ : Ref[Boolean], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    isempty_h(buf, n, ENCODED_REF_NODE_SIZE, empty_)
    if (empty_.get) {
      err := types.error.ESUCCESS
    } else {
      val m = Ref[Int](0)
      decode_ref_node_empty(n, buf, a, m, err)
    }
  }
  def decode_superblock(n: Int, buf: buffer, m: Int, a: superblock, n0: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    decode_superblock_unaligned(n, buf, a, n0, err)
    if (err.get == types.error.ESUCCESS) {
      n0 := alignUp(n0.get, m)
      if (! (n + n0.get <= buf.length)) {
        err := types.error.EINVAL
      }
    }
  }
  def decode_vidheader(n: Int, buf: buffer, a: Ref[vidheader], empty_ : Ref[Boolean], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    decode_vidheader_nonempty(0, buf, a, empty_, err)
    if (err.get == types.error.ESUCCESS && empty_.get) {
      isempty_h(buf, ENCODED_VIDHEADER_SIZE, n - ENCODED_VIDHEADER_SIZE, empty_)
      if (empty_.get != true) {
        err := types.error.EINVAL
      }
    }
  }
  def decode_vidheader_nonempty(n: Int, buf: buffer, a: Ref[vidheader], empty_ : Ref[Boolean], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    isempty_h(buf, n, ENCODED_VIDHEADER_SIZE, empty_)
    if (empty_.get) {
      err := types.error.ESUCCESS
    } else {
      val m = Ref[Int](0)
      decode_vidheader_empty(n, buf, a, m, err)
    }
  }
  def decode_vtbl(BUF: buffer, VTBL: vtbl, ERR: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    VTBL.clear
    ERR := types.error.ESUCCESS
    var INDEX: Int = 0
    val SIZE = Ref[Int](0)
    val N = Ref[Int](0)
    decode_nat(INDEX, BUF, N, SIZE, ERR)
    INDEX = INDEX + SIZE.get
    while (N.get != 0 && ERR.get == types.error.ESUCCESS) {
      val VOLID = Ref[Byte](0.toByte)
      decode_volid(INDEX, BUF, VOLID, SIZE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        INDEX = INDEX + SIZE.get
        val M = Ref[Int](0)
        decode_nat(INDEX, BUF, M, SIZE, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          INDEX = INDEX + SIZE.get
          VTBL(VOLID.get) = M.get
        }
      }
      N := N.get - 1
    }
  }
  def encode_echeader(n: Int, a: echeader, buf: buffer, err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    encode_echeader_nonempty(a, 0, buf, err)
  }
  def encode_echeader_nonempty(a: echeader, n: Int, buf: buffer, err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    val m = Ref[Int](0)
    encode_echeader_empty(a, n, buf, m, err)
    if (err.get == types.error.ESUCCESS) {
      val empty_ = Ref[Boolean](helpers.scala.Boolean.uninit)
      isempty_h(buf, n, m.get, empty_)
      if (empty_.get) {
        debug("encoding_nonempty: encoding is empty")
        err := types.error.EINVAL
      }
    }
  }
  def encode_group_node(ND: group_node, OFFSET: Int, SIZE: Ref[Int], BUF: buffer, ERR: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    val NDSIZE: Int = group_node_size_headerless(ND)
    val ALIGNEDSIZE: Int = alignUp(NDSIZE, 2 * NODE_HEADER_SIZE)
    encode_header(types.node_header.nodeheader(NDSIZE, false), OFFSET, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val SIZE0 = Ref[Int](0)
      encode_group_node_headerless(ND, OFFSET + NODE_HEADER_SIZE, BUF, SIZE0, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      BUF.fill(empty, OFFSET + (NODE_HEADER_SIZE + NDSIZE), ALIGNEDSIZE - NDSIZE)
      BUF.copy(validtrailer, 0, OFFSET + (NODE_HEADER_SIZE + ALIGNEDSIZE), NODE_HEADER_SIZE)
    }
    SIZE := 2 * NODE_HEADER_SIZE + ALIGNEDSIZE
  }
  def encode_header(a: node_header, n: Int, buf: buffer, err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    val m = Ref[Int](0)
    encode_header_empty(a, n, buf, m, err)
    if (err.get == types.error.ESUCCESS) {
      val empty_ = Ref[Boolean](helpers.scala.Boolean.uninit)
      isempty_h(buf, n, m.get, empty_)
      if (empty_.get) {
        debug("encoding_nonempty: encoding is empty")
        err := types.error.EINVAL
      }
    }
  }
  def encode_index_node(ND: index_node, OFFSET: Int, SIZE: Ref[Int], BUF: buffer, ERR: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    val NDSIZE: Int = index_node_size_headerless(ND)
    val ALIGNEDSIZE: Int = alignUp(NDSIZE, 2 * NODE_HEADER_SIZE)
    encode_header(types.node_header.nodeheader(NDSIZE, false), OFFSET, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val SIZE0 = Ref[Int](0)
      encode_index_node_headerless(ND, OFFSET + NODE_HEADER_SIZE, BUF, SIZE0, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      BUF.fill(empty, OFFSET + (NODE_HEADER_SIZE + NDSIZE), ALIGNEDSIZE - NDSIZE)
      BUF.copy(validtrailer, 0, OFFSET + (NODE_HEADER_SIZE + ALIGNEDSIZE), NODE_HEADER_SIZE)
    }
    SIZE := 2 * NODE_HEADER_SIZE + ALIGNEDSIZE
  }
  def encode_ref_node(n: Int, a: ref_node, buf: buffer, err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    encode_ref_node_nonempty(a, 0, buf, err)
  }
  def encode_ref_node_nonempty(a: ref_node, n: Int, buf: buffer, err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    val m = Ref[Int](0)
    encode_ref_node_empty(a, n, buf, m, err)
    if (err.get == types.error.ESUCCESS) {
      val empty_ = Ref[Boolean](helpers.scala.Boolean.uninit)
      isempty_h(buf, n, m.get, empty_)
      if (empty_.get) {
        debug("encoding_nonempty: encoding is empty")
        err := types.error.EINVAL
      }
    }
  }
  def encode_superblock(a: superblock, n: Int, m: Int, buf: buffer, n0: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    encode_superblock_unaligned(a, n, buf, n0, err)
    if (err.get == types.error.ESUCCESS) {
      n0 := alignUp(n0.get, m)
    }
  }
  def encode_vidheader(n: Int, a: vidheader, buf: buffer, err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    encode_vidheader_nonempty(a, 0, buf, err)
  }
  def encode_vidheader_nonempty(a: vidheader, n: Int, buf: buffer, err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    val m = Ref[Int](0)
    encode_vidheader_empty(a, n, buf, m, err)
    if (err.get == types.error.ESUCCESS) {
      val empty_ = Ref[Boolean](helpers.scala.Boolean.uninit)
      isempty_h(buf, n, m.get, empty_)
      if (empty_.get) {
        debug("encoding_nonempty: encoding is empty")
        err := types.error.EINVAL
      }
    }
  }
  def encode_vtbl(VTBL: vtbl, BUF: buffer, ERR: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    ERR := types.error.ESUCCESS
    var INDEX: Int = 0
    val SIZE = Ref[Int](0)
    encode_nat(VTBL.size, INDEX, BUF, SIZE, ERR)
    INDEX = INDEX + SIZE.get
    while (ERR.get == types.error.ESUCCESS && ! VTBL.isEmpty) {
      val VOLID: Byte = VTBL.headKey
      encode_volid(VOLID, INDEX, BUF, SIZE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        INDEX = INDEX + SIZE.get
        encode_nat(VTBL(VOLID), INDEX, BUF, SIZE, ERR)
        INDEX = INDEX + SIZE.get
      }
      VTBL -= VOLID
    }
  }
  def gc_heap_empty(bh: binheap)  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    bh := types.binheap.bin_heap(new key_array(1).fill(types.keyindex.uninit), 0)
  }
  def gc_heap_get_min(bh: binheap, n: Ref[Int])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    if (bh.size == 0)
      assert(false, """abort""")
    
    n := bh.ar(0).idx
  }
  def gc_heap_insert(ki: keyindex, bh: binheap, ar: gc_array)  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    if (! (ki.idx < ar.length))
      assert(false, """abort""")
    
    bin_min_heap_resize(bh.size + 1, bh)
    bh.ar(bh.size) = ki.deepCopy
    ar(ki.idx) = bh.size
    bh.size = bh.size + 1
    bin_min_heap_bubble_up(bh.size - 1, bh, ar)
  }
  def gc_heap_is_empty(bh: binheap, isempty: Ref[Boolean])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    isempty := (bh.size == 0)
  }
  def gc_heap_remove(n: Int, bh: binheap, ar: gc_array)  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    if (! (n < bh.size))
      assert(false, """abort""")
    
    bh.size = bh.size - 1
    if (bh.size != 0 && bh.size != n) {
      bh.ar(n) = bh.ar(bh.size).deepCopy
      ar(bh.ar(n).idx) = n
      if (n != 0 && bh.ar(n).key < bh.ar((n - 1) / 2).key) {
        bin_min_heap_bubble_up(n, bh, ar)
      } else {
        bin_min_heap_bubble_down(n, bh, ar)
      }
    }
  }
  def gc_heap_update(n: Int, key: Int, bh: binheap, ar: gc_array)  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    if (! (n < bh.size))
      assert(false, """abort""")
    
    val key0: Int = bh.ar(n).key
    bh.ar(n).key = key
    if (key < key0) {
      bin_min_heap_bubble_up(n, bh, ar)
    } else {
      bin_min_heap_bubble_down(n, bh, ar)
    }
  }
  def gjournal_split_nodes(SIZE1: Int, NDLIST: node_list, NDLIST0: node_list)  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    var SIZE: Int = SIZE1
    NDLIST0.clear
    while (! NDLIST.isEmpty && flashsize(NDLIST.head) <= SIZE) {
      val ND: node = NDLIST.head.deepCopy
      NDLIST0 += ND
      NDLIST.removeHead
      SIZE = SIZE - flashsize(ND)
    }
  }
  def isempty_h(Buf: buffer, N0: Int, N2: Int, Boolvar: Ref[Boolean])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    var N1: Int = N2
    while (N1 != 0 && Buf((N0 + N1) - 1) == empty) {
      N1 = N1 - 1
    }
    Boolvar := (N1 == 0)
  }
  def persistence_encode_group_nodes(NODELIST: group_node_list, LNUM: Int, N: Int, BUF: buffer, ADRLIST: address_list, ERR: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    ERR := types.error.ESUCCESS
    BUF.allocate(flashsize(NODELIST), 0.toByte)
    ADRLIST.clear
    var IDX: Int = 0
    var OFFSET: Int = 0
    while (ERR.get == types.error.ESUCCESS && IDX < NODELIST.length) {
      val SIZE = Ref[Int](0)
      encode_group_node(NODELIST(IDX), OFFSET, SIZE, BUF, ERR)
      ADRLIST += types.address.at(LNUM, N + OFFSET, SIZE.get)
      OFFSET = OFFSET + SIZE.get
      IDX = IDX + 1
    }
  }
  def persistence_encode_padding_node(BUF: buffer, ERR: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    val SIZE: Int = BUF.length - 2 * NODE_HEADER_SIZE
    encode_header(types.node_header.nodeheader(SIZE, true), 0, BUF, ERR)
    BUF.copy(validtrailer, 0, BUF.length - NODE_HEADER_SIZE, NODE_HEADER_SIZE)
  }
  def persistence_io_decode_lpt(BUF: buffer, LPT: lp_array, ERR: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    ERR := types.error.ESUCCESS
    var N: Int = 0
    while (ERR.get == types.error.ESUCCESS && N < LPT.length) {
      val SIZE = Ref[Int](0)
      decode_lprops(N * ENCODED_LPROPS_SIZE, BUF, LPT(N), SIZE, ERR)
      N = N + 1
    }
  }
  def persistence_io_decode_orphans(SIZE2: Int, BUF: buffer, ORPHANS: nat_set, ERR: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    var SIZE: Int = SIZE2
    ORPHANS.clear
    ERR := types.error.ESUCCESS
    var OFFSET: Int = 0
    while (ERR.get == types.error.ESUCCESS && SIZE != 0) {
      val N = Ref[Int](0)
      val SIZE1 = Ref[Int](0)
      decode_nat(OFFSET, BUF, N, SIZE1, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        ORPHANS += N.get
      }
      OFFSET = OFFSET + ENCODED_NAT_SIZE
      SIZE = SIZE - 1
    }
  }
  def rangeeq(ar0: buffer, n0: Int, ar1: buffer, n1: Int, n2: Int, boolvar: Ref[Boolean])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    var n: Int = n2
    if (n0 + n <= ar0.length && n1 + n <= ar1.length) {
      while (n != 0 && ar0((n0 + n) - 1) == ar1((n1 + n) - 1)) {
        n = n - 1
      }
      boolvar := (n == 0)
    } else {
      boolvar := false
    }
  }
}
