// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import encoding.lprops._
import encoding.ref_node._
import encoding.superblock._
import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import proc._
import types._
import types.error.error
import types.lpropflags.lpropflags

class persistence_io_asm(val SB : superblock, var LOGOFF : Int, val awbuf : awbuf_interface)(implicit _algebraic_implicit: algebraic.Algebraic, _procedures_implicit: proc.Procedures) extends apersistence_io_interface {
  import _algebraic_implicit._
  import _procedures_implicit._

  override def apersistence_io_add_log_leb(LNUM: Int, ERR: Ref[error]): Unit = {
    if (! (LOGOFF + EB_PAGE_SIZE <= LEB_SIZE))
      ERR := types.error.ECOMMIT
    else {
      val BUF: buffer = new buffer(EB_PAGE_SIZE).fill(0.toByte)
      encode_ref_node(types.ref_node.rnode(LNUM), BUF, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        awbuf.awbuf_write(SB.log, LOGOFF, 0, EB_PAGE_SIZE, BUF, ERR)
        if (ERR.get == types.error.ESUCCESS)
          LOGOFF = LOGOFF + EB_PAGE_SIZE
        else {
          LOGOFF = LEB_SIZE
          debug("persistence-io: failed to add LEB " + (toStr(LNUM) + " to log"))
        }
      }
    }
  }

  override def apersistence_io_commit(LPT: lp_array, INDEXADR0: address, MAXINO0: Int, ORPHANS: nat_set, ERR: Ref[error]): Unit = {
    val VOLSIZE = new Ref[Int](0)
    awbuf.awbuf_get_volume_size(VOLSIZE)
    VOLSIZE := VOLSIZE.get - SB.main
    val NEWSB: superblock = SB.deepCopy
    NEWSB.indexaddr = INDEXADR0
    NEWSB.maxino = MAXINO0
    NEWSB.orphsize = ORPHANS.size
    if (NEWSB.log == SB_LOG) {
      NEWSB.log = SB_LOG + 1
      NEWSB.orph = SB_ORPH + 1
      NEWSB.lpt = SB_LPT + lptlebs(VOLSIZE.get)
    } else {
      NEWSB.log = SB_LOG
      NEWSB.orph = SB_ORPH
      NEWSB.lpt = SB_LPT
    }
    awbuf.awbuf_remap(NEWSB.log, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      persistence_io_write_orphans(NEWSB.orph, ORPHANS, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      val BUF: buffer = new buffer()
      persistence_io_encode_lpt(LPT, BUF, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        persistence_io_write_lebs(NEWSB.lpt, lptlebs(VOLSIZE.get), BUF, ERR)
      }
    }
    if (ERR.get == types.error.ESUCCESS) {
      awbuf.awbuf_sync_device(ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      persistence_io_write_superblock(NEWSB, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      SB := NEWSB
      LOGOFF = 0
    }
  }

  override def apersistence_io_create_buf(LNUM: Int, OFFSET: Int): Unit = {
    awbuf.awbuf_create_buf(SB.main + LNUM, OFFSET)
  }

  override def apersistence_io_destroy_buf(LNUM: Int): Unit = {
    awbuf.awbuf_destroy_buf(SB.main + LNUM)
  }

  override def apersistence_io_destroy_bufs(): Unit = {
    awbuf.awbuf_destroy_bufs()
  }

  override def apersistence_io_format(VOLSIZE: Int, LPT: lp_array, INDEXADR0: address, MAXINO0: Int, ERR: Ref[error]): Unit = {
    val N: Int = 5 + 2 * lptlebs(VOLSIZE)
    awbuf.awbuf_format(VOLSIZE + N, ERR)
    if (ERR.get != types.error.ESUCCESS) {
      debug("persistence-io: wbuf format failed")
    }
    SB.indexaddr = INDEXADR0
    SB.maxino = MAXINO0
    SB.log = SB_LOG
    SB.orph = SB_ORPH
    SB.orphsize = 0
    SB.lpt = SB_LPT
    SB.main = N
    if (ERR.get == types.error.ESUCCESS) {
      awbuf.awbuf_remap(SB.log, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      awbuf.awbuf_remap(SB.orph, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      val BUF: buffer = new buffer()
      persistence_io_encode_lpt(LPT, BUF, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        persistence_io_write_lebs(SB.lpt, lptlebs(VOLSIZE), BUF, ERR)
      }
    }
    if (ERR.get == types.error.ESUCCESS) {
      persistence_io_write_superblock(SB, ERR)
    }
    LOGOFF = LEB_SIZE
  }

  override def apersistence_io_get_bufs(WBS0: nat_set): Unit = {
    val WBS1: nat_set = new nat_set()
    awbuf.awbuf_get_bufs(WBS1)
    WBS0 := minus(WBS1, SB.main).deepCopy
  }

  override def apersistence_io_get_volume_size(N: Ref[Int]): Unit = {
    awbuf.awbuf_get_volume_size(N)
    N := N.get - SB.main
  }

  override def apersistence_io_is_buffered(LNUM: Int, ISBUF: Ref[Boolean]): Unit = {
    awbuf.awbuf_is_buffered(SB.main + LNUM, ISBUF)
  }

  override def apersistence_io_read_buf(LNUM: Int, OFFSET: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    awbuf.awbuf_read_buf(SB.main + LNUM, OFFSET, N, BUF, ERR)
  }

  override def apersistence_io_recover(INDEXADR0: Ref[address], MAXINO0: Ref[Int], ORPHANS: nat_set, LOG: nat_list, LPT: lp_array, ERR: Ref[error]): Unit = {
    awbuf.awbuf_recover(ERR)
    if (ERR.get != types.error.ESUCCESS) {
      debug("persistence-io: wbuf recover failed")
    } else {
      val VOLSIZE = new Ref[Int](0)
      awbuf.awbuf_get_volume_size(VOLSIZE)
      persistence_io_read_superblock(SB, ERR)
      if (ERR.get != types.error.ESUCCESS) {
        debug("persistence-io: reading superblock failed")
      }
      if (ERR.get == types.error.ESUCCESS) {
        persistence_io_read_log(LOG, ERR)
        if (ERR.get != types.error.ESUCCESS) {
          debug("persistence-io: reading log failed")
        }
      }
      if (ERR.get == types.error.ESUCCESS) {
        persistence_io_read_orphans(ORPHANS, ERR)
        if (ERR.get != types.error.ESUCCESS) {
          debug("persistence-io: reading orphans failed")
        }
      }
      VOLSIZE := VOLSIZE.get - SB.main
      if (ERR.get == types.error.ESUCCESS) {
        val BUF: buffer = new buffer()
        persistence_io_read_lebs(SB.lpt, lptlebs(VOLSIZE.get), BUF, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          LPT.allocate(VOLSIZE.get, types.lprops.uninit)
          LPT.fill(types.lprops.mklp(0, 0, types.lpropflags.LP_FREE, 0))
          persistence_io_decode_lpt(BUF, LPT, ERR)
        }
        if (ERR.get != types.error.ESUCCESS) {
          debug("persistence-io: reading LPT failed")
        }
      }
      if (ERR.get == types.error.ESUCCESS) {
        LOGOFF = LEB_SIZE
        INDEXADR0 := SB.indexaddr
        MAXINO0 := SB.maxino
      }
    }
  }

  override def apersistence_io_remap(LNUM: Int, ERR: Ref[error]): Unit = {
    awbuf.awbuf_remap(SB.main + LNUM, ERR)
  }

  override def apersistence_io_requires_commit(COMMIT_ : Ref[Boolean]): Unit = {
    COMMIT_ := (LOGOFF == LEB_SIZE)
  }

  override def apersistence_io_unmap(LNUM: Int): Unit = {
    awbuf.awbuf_unmap(SB.main + LNUM)
  }

  override def apersistence_io_write_buf(LNUM: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    awbuf.awbuf_write_buf(SB.main + LNUM, N, BUF, ERR)
  }

  def decode_ref_node(buf: buffer, rnd: Ref[ref_node], err: Ref[error]): Unit = {
    if (ENCODED_REF_NODE_SIZE <= EB_PAGE_SIZE) {
      decode_ref_node_nonempty(0, buf, rnd, err)
    } else
      err := types.error.EINVAL
  }

  def decode_ref_node_nonempty(n: Int, buf: buffer, rnd: Ref[ref_node], err: Ref[error]): Unit = {
    if (isempty(buf, n, ENCODED_REF_NODE_SIZE))
      err := types.error.EINVAL
    else {
      val m = new Ref[Int](0)
      decode_ref_node_empty(n, buf, rnd, m, err)
    }
  }

  def decode_superblock(n: Int, buf: buffer, sb: superblock, n0: Ref[Int], err: Ref[error]): Unit = {
    decode_superblock_unaligned(n, buf, sb, n0, err)
    if (err.get == types.error.ESUCCESS) {
      n0 := alignUp(n0.get, EB_PAGE_SIZE)
      if (! (n + n0.get <= buf.length))
        err := types.error.EINVAL
      
    }
  }

  def encode_ref_node(rnd: ref_node, buf: buffer, err: Ref[error]): Unit = {
    if (ENCODED_REF_NODE_SIZE <= EB_PAGE_SIZE) {
      encode_ref_node_nonempty(rnd, 0, buf, err)
    } else
      err := types.error.EINVAL
  }

  def encode_ref_node_nonempty(rnd: ref_node, n: Int, buf: buffer, err: Ref[error]): Unit = {
    val m = new Ref[Int](0)
    encode_ref_node_empty(rnd, n, buf, m, err)
    if (err.get == types.error.ESUCCESS && isempty(buf, n, m.get)) {
      debug("encoding_nonempty: encoding is empty")
      err := types.error.EINVAL
    }
  }

  def encode_superblock(sb: superblock, n: Int, buf: buffer, n0: Ref[Int], err: Ref[error]): Unit = {
    encode_superblock_unaligned(sb, n, buf, n0, err)
    if (err.get == types.error.ESUCCESS)
      n0 := alignUp(n0.get, EB_PAGE_SIZE)
    
  }

  def persistence_io_decode_lpt(BUF: buffer, LPT: lp_array, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    var N: Int = 0
    while (ERR.get == types.error.ESUCCESS && N < LPT.length) {
      val SIZE = new Ref[Int](0)
      decode_lprops(N * ENCODED_LPROPS_SIZE, BUF, LPT(N), SIZE, ERR)
      N = N + 1
    }
  }

  def persistence_io_decode_orphans(N0: Int, BUF: buffer, ORPHANS: nat_set, ERR: Ref[error]): Unit = {
    var SIZE: Int = N0
    ORPHANS.clear
    ERR := types.error.ESUCCESS
    var OFFSET: Int = 0
    while (ERR.get == types.error.ESUCCESS && SIZE != 0) {
      val N = new Ref[Int](0)
      decode_nat(OFFSET, BUF, N, N, ERR)
      if (ERR.get == types.error.ESUCCESS)
        ORPHANS += N.get
      
      OFFSET = OFFSET + ENCODED_NAT_SIZE
      SIZE = SIZE - 1
    }
  }

  def persistence_io_encode_lpt(LPT: lp_array, BUF: buffer, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    BUF.allocate(lptsize(LPT.length), 0.toByte)
    var N: Int = 0
    val SIZE = new Ref[Int](0)
    while (ERR.get == types.error.ESUCCESS && N < LPT.length) {
      encode_lprops(LPT(N), N * ENCODED_LPROPS_SIZE, BUF, SIZE, ERR)
      N = N + 1
    }
  }

  def persistence_io_encode_orphans(ORPHANS: nat_set, BUF: buffer, ERR: Ref[error]): Unit = {
    if (ORPHANS.size * ENCODED_NAT_SIZE > LEB_SIZE)
      ERR := types.error.ENOSPC
    else {
      ERR := types.error.ESUCCESS
      BUF.allocate(alignUp(ORPHANS.size * ENCODED_NAT_SIZE, EB_PAGE_SIZE), 0.toByte)
      var OFFSET: Int = 0
      val SIZE = new Ref[Int](0)
      while (ERR.get == types.error.ESUCCESS && ! ORPHANS.isEmpty) {
        val N: Int = ORPHANS.head
        encode_nat(N, OFFSET, BUF, SIZE, ERR)
        OFFSET = OFFSET + ENCODED_NAT_SIZE
        ORPHANS -= N
      }
    }
  }

  def persistence_io_read_lebs(LNUM: Int, SIZE: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    BUF.allocate(SIZE * LEB_SIZE, 0.toByte)
    var N: Int = 0
    while (ERR.get == types.error.ESUCCESS && N < SIZE) {
      awbuf.awbuf_read(LNUM + N, 0, N * LEB_SIZE, LEB_SIZE, BUF, ERR)
      N = N + 1
    }
  }

  def persistence_io_read_log(LOG: nat_list, ERR: Ref[error]): Unit = {
    LOG.clear
    ERR := types.error.ESUCCESS
    val BUF: buffer = new buffer(EB_PAGE_SIZE).fill(0.toByte)
    var LOGOFF: Int = 0
    while (ERR.get == types.error.ESUCCESS && LOGOFF < LEB_SIZE) {
      awbuf.awbuf_read(SB.log, LOGOFF, 0, EB_PAGE_SIZE, BUF, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (isempty(BUF))
          ERR := types.error.ENOENT
        else {
          val RNODE = new Ref[ref_node](types.ref_node.uninit)
          decode_ref_node(BUF, RNODE, ERR)
          if (ERR.get == types.error.ESUCCESS)
            LOG += RNODE.get.leb
          
        }
      }
      LOGOFF = LOGOFF + EB_PAGE_SIZE
    }
    if (ERR.get == types.error.ENOENT)
      ERR := types.error.ESUCCESS
    
  }

  def persistence_io_read_orphans(ORPHANS: nat_set, ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(SB.orphsize * ENCODED_NAT_SIZE).fill(0.toByte)
    awbuf.awbuf_read(SB.orph, 0, 0, SB.orphsize * ENCODED_NAT_SIZE, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      persistence_io_decode_orphans(SB.orphsize, BUF, ORPHANS, ERR)
    }
  }

  def persistence_io_read_superblock(NEWSB: superblock, ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(LEB_SIZE).fill(0.toByte)
    awbuf.awbuf_read(0, 0, 0, LEB_SIZE, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val SIZE = new Ref[Int](0)
      decode_superblock(0, BUF, NEWSB, SIZE, ERR)
    }
  }

  def persistence_io_write_lebs(LNUM: Int, SIZE: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    var N: Int = 0
    while (ERR.get == types.error.ESUCCESS && N < SIZE) {
      awbuf.awbuf_remap(LNUM + N, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        awbuf.awbuf_write(LNUM + N, 0, N * LEB_SIZE, LEB_SIZE, BUF, ERR)
      }
      N = N + 1
    }
  }

  def persistence_io_write_orphans(LNUM: Int, ORPHANS: nat_set, ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(0).fill(0.toByte)
    persistence_io_encode_orphans(ORPHANS, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      awbuf.awbuf_remap(LNUM, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      awbuf.awbuf_write(LNUM, 0, 0, BUF.length, BUF, ERR)
    }
  }

  def persistence_io_write_superblock(NEWSB: superblock, ERR: Ref[error]): Unit = {
    val SIZE = new Ref[Int](size(NEWSB))
    if (SIZE.get <= LEB_SIZE) {
      val BUF: buffer = new buffer(SIZE.get).fill(0.toByte)
      encode_superblock(NEWSB, 0, BUF, SIZE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        awbuf.awbuf_change(0, SIZE.get, BUF, ERR)
      }
    } else
      ERR := types.error.ENOSPC
  }

}

