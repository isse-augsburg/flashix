// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
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

class persistence_io_asm(var LEBSIZE : Int, var LOGOFF : Int, var PAGESIZE : Int, val SB : superblock, val ebm_avol : ebm_avol_interface)(implicit _algebraic_implicit: algebraic.Algebraic, _procedures_implicit: proc.Procedures) extends apersistence_io_interface {
  import _algebraic_implicit._
  import _procedures_implicit._

  override def add_log_leb(LNUM: Int, ERR: Ref[error]): Unit = {
    if (! (LOGOFF + PAGESIZE <= LEBSIZE)) {
      ERR := types.error.ECOMMIT
    } else {
      val BUF: buffer = new buffer(PAGESIZE).fill(0.toByte)
      encode_ref_node(PAGESIZE, types.ref_node.rnode(LNUM), BUF, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        ebm_avol.write(SB.log, LOGOFF, 0, PAGESIZE, BUF, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          LOGOFF = LOGOFF + PAGESIZE
        } else {
          LOGOFF = LEBSIZE
          debug("persistence-io: failed to add LEB " + (toStr(LNUM) + " to log"))
        }
      }
    }
  }

  override def commit(LPT: lp_array, INDEXADR0: address, MAXINO0: Int, ORPHANS: nat_set, ERR: Ref[error]): Unit = {
    val VOLSIZE = Ref[Int](0)
    ebm_avol.get_volume_size(VOLSIZE)
    VOLSIZE := VOLSIZE.get - SB.main
    val NEWSB: superblock = SB.deepCopy
    NEWSB.indexaddr = INDEXADR0
    NEWSB.maxino = MAXINO0
    NEWSB.orphsize = ORPHANS.size
    if (NEWSB.log == SB_LOG) {
      NEWSB.log = SB_LOG + 1
      NEWSB.orph = SB_ORPH + 1
      NEWSB.lpt = SB_LPT + lptlebs(VOLSIZE.get, LEBSIZE)
    } else {
      NEWSB.log = SB_LOG
      NEWSB.orph = SB_ORPH
      NEWSB.lpt = SB_LPT
    }
    ebm_avol.remap(NEWSB.log, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      write_orphans(NEWSB.orph, ORPHANS, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      val BUF: buffer = new buffer()
      encode_lpt(LPT, BUF, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        write_lebs(NEWSB.lpt, lptlebs(VOLSIZE.get, LEBSIZE), BUF, ERR)
      }
    }
    if (ERR.get == types.error.ESUCCESS) {
      ebm_avol.sync_device(ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      write_superblock(NEWSB, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      SB := NEWSB
      LOGOFF = 0
    }
  }

  def decode_lpt(BUF: buffer, LPT: lp_array, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    var N: Int = 0
    while (ERR.get == types.error.ESUCCESS && N < LPT.length) {
      val SIZE = Ref[Int](0)
      decode_lprops(N * ENCODED_LPROPS_SIZE, BUF, LPT(N), SIZE, ERR)
      N = N + 1
    }
  }

  def decode_orphans(P_INO: Int, BUF: buffer, ORPHANS: nat_set, ERR: Ref[error]): Unit = {
    var SIZE: Int = P_INO
    ORPHANS.clear
    ERR := types.error.ESUCCESS
    var OFFSET: Int = 0
    while (ERR.get == types.error.ESUCCESS && SIZE != 0) {
      val N = Ref[Int](0)
      val OLD_INO = Ref[Int](0)
      decode_nat(OFFSET, BUF, N, OLD_INO, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        ORPHANS += N.get
      }
      OFFSET = OFFSET + ENCODED_NAT_SIZE
      SIZE = SIZE - 1
    }
  }

  def decode_ref_node(n: Int, buf: buffer, rnd: Ref[ref_node], err: Ref[error]): Unit = {
    if (ENCODED_REF_NODE_SIZE <= n) {
      decode_ref_node_nonempty(0, buf, rnd, err)
    } else {
      err := types.error.EINVAL
    }
  }

  def decode_ref_node_nonempty(n: Int, buf: buffer, rnd: Ref[ref_node], err: Ref[error]): Unit = {
    if (isempty(buf, n, ENCODED_REF_NODE_SIZE)) {
      err := types.error.EINVAL
    } else {
      val m = Ref[Int](0)
      decode_ref_node_empty(n, buf, rnd, m, err)
    }
  }

  def decode_superblock(n: Int, buf: buffer, m: Int, sb: superblock, n0: Ref[Int], err: Ref[error]): Unit = {
    decode_superblock_unaligned(n, buf, sb, n0, err)
    if (err.get == types.error.ESUCCESS) {
      n0 := alignUp(n0.get, m)
      if (! (n + n0.get <= buf.length)) {
        err := types.error.EINVAL
      }
    }
  }

  def encode_lpt(LPT: lp_array, BUF: buffer, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    BUF.allocate(lptsize(LPT.length, LEBSIZE), 0.toByte)
    var N: Int = 0
    val SIZE = Ref[Int](0)
    while (ERR.get == types.error.ESUCCESS && N < LPT.length) {
      encode_lprops(LPT(N), N * ENCODED_LPROPS_SIZE, BUF, SIZE, ERR)
      N = N + 1
    }
  }

  def encode_orphans(ORPHANS: nat_set, BUF: buffer, ERR: Ref[error]): Unit = {
    if (ORPHANS.size * ENCODED_NAT_SIZE > LEBSIZE) {
      ERR := types.error.ENOSPC
    } else {
      ERR := types.error.ESUCCESS
      BUF.allocate(alignUp(ORPHANS.size * ENCODED_NAT_SIZE, PAGESIZE), 0.toByte)
      var OFFSET: Int = 0
      val SIZE = Ref[Int](0)
      while (ERR.get == types.error.ESUCCESS && ! ORPHANS.isEmpty) {
        val N: Int = ORPHANS.head
        encode_nat(N, OFFSET, BUF, SIZE, ERR)
        OFFSET = OFFSET + ENCODED_NAT_SIZE
        ORPHANS -= N
      }
    }
  }

  def encode_ref_node(n: Int, rnd: ref_node, buf: buffer, err: Ref[error]): Unit = {
    if (ENCODED_REF_NODE_SIZE <= n) {
      encode_ref_node_nonempty(rnd, 0, buf, err)
    } else {
      err := types.error.EINVAL
    }
  }

  def encode_ref_node_nonempty(rnd: ref_node, n: Int, buf: buffer, err: Ref[error]): Unit = {
    val m = Ref[Int](0)
    encode_ref_node_empty(rnd, n, buf, m, err)
    if (err.get == types.error.ESUCCESS && isempty(buf, n, m.get)) {
      debug("encoding_nonempty: encoding is empty")
      err := types.error.EINVAL
    }
  }

  def encode_superblock(sb: superblock, n: Int, m: Int, buf: buffer, n0: Ref[Int], err: Ref[error]): Unit = {
    encode_superblock_unaligned(sb, n, buf, n0, err)
    if (err.get == types.error.ESUCCESS) {
      n0 := alignUp(n0.get, m)
    }
  }

  override def format(VOLSIZE: Int, SIZE: Int, LPT: lp_array, INDEXADR0: address, MAXINO0: Int, ERR: Ref[error]): Unit = {
    val N: Int = 5 + 2 * lptlebs(VOLSIZE, SIZE)
    ebm_avol.format(VOLSIZE + N, ERR)
    if (ERR.get != types.error.ESUCCESS) {
      debug("persistence-io: ubi format failed")
    }
    if (ERR.get == types.error.ESUCCESS) {
      
      {
        val pageno: Ref[Int] = Ref[Int](LEBSIZE)
        ebm_avol.get_leb_size(pageno)
        LEBSIZE = pageno.get
      }
    }
    SB.indexaddr = INDEXADR0
    SB.maxino = MAXINO0
    SB.log = SB_LOG
    SB.orph = SB_ORPH
    SB.orphsize = 0
    SB.lpt = SB_LPT
    SB.main = N
    if (ERR.get == types.error.ESUCCESS) {
      
      {
        val pageno: Ref[Int] = Ref[Int](PAGESIZE)
        ebm_avol.get_page_size(pageno)
        PAGESIZE = pageno.get
      }
    }
    if (ERR.get == types.error.ESUCCESS) {
      ebm_avol.remap(SB.log, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      ebm_avol.remap(SB.orph, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      val BUF: buffer = new buffer()
      encode_lpt(LPT, BUF, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        write_lebs(SB.lpt, lptlebs(VOLSIZE, LEBSIZE), BUF, ERR)
      }
    }
    if (ERR.get == types.error.ESUCCESS) {
      write_superblock(SB, ERR)
    }
    LOGOFF = LEBSIZE
  }

  override def get_leb_size(N: Ref[Int]): Unit = {
    N := LEBSIZE
  }

  override def get_page_size(N: Ref[Int]): Unit = {
    N := PAGESIZE
  }

  override def get_volume_size(N: Ref[Int]): Unit = {
    ebm_avol.get_volume_size(N)
    N := N.get - SB.main
  }

  override def read(LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    ebm_avol.read(SB.main + LNUM, OFFSET, N0, N, BUF, ERR)
  }

  def read_lebs(LNUM: Int, SIZE: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    BUF.allocate(SIZE * LEBSIZE, 0.toByte)
    var N: Int = 0
    while (ERR.get == types.error.ESUCCESS && N < SIZE) {
      ebm_avol.read(LNUM + N, 0, N * LEBSIZE, LEBSIZE, BUF, ERR)
      N = N + 1
    }
  }

  def read_log(LOG: nat_list, ERR: Ref[error]): Unit = {
    LOG.clear
    ERR := types.error.ESUCCESS
    LOGOFF = 0
    val BUF: buffer = new buffer(PAGESIZE).fill(0.toByte)
    while (ERR.get == types.error.ESUCCESS && LOGOFF < LEBSIZE) {
      ebm_avol.read(SB.log, LOGOFF, 0, PAGESIZE, BUF, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (isempty(BUF)) {
          ERR := types.error.ENOENT
        } else {
          val RND = Ref[ref_node](types.ref_node.uninit)
          decode_ref_node(PAGESIZE, BUF, RND, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            LOG += RND.get.leb
          }
        }
      }
      LOGOFF = LOGOFF + PAGESIZE
    }
    if (ERR.get == types.error.ENOENT) {
      ERR := types.error.ESUCCESS
    }
  }

  def read_orphans(ORPHANS: nat_set, ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(SB.orphsize * ENCODED_NAT_SIZE).fill(0.toByte)
    ebm_avol.read(SB.orph, 0, 0, SB.orphsize * ENCODED_NAT_SIZE, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      decode_orphans(SB.orphsize, BUF, ORPHANS, ERR)
    }
  }

  def read_superblock(NEWSB: superblock, ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(LEBSIZE).fill(0.toByte)
    ebm_avol.read(0, 0, 0, LEBSIZE, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val SIZE = Ref[Int](0)
      decode_superblock(0, BUF, PAGESIZE, NEWSB, SIZE, ERR)
    }
  }

  override def recover(INDEXADR0: Ref[address], MAXINO0: Ref[Int], ORPHANS: nat_set, LOG: nat_list, LPT: lp_array, ERR: Ref[error]): Unit = {
    ebm_avol.recover(ERR)
    if (ERR.get != types.error.ESUCCESS) {
      debug("persistence-io: ubi recover failed")
    } else {
      
      {
        val pageno: Ref[Int] = Ref[Int](LEBSIZE)
        ebm_avol.get_leb_size(pageno)
        LEBSIZE = pageno.get
      }
      
      {
        val pageno: Ref[Int] = Ref[Int](PAGESIZE)
        ebm_avol.get_page_size(pageno)
        PAGESIZE = pageno.get
      }
      val VOLSIZE = Ref[Int](0)
      ebm_avol.get_volume_size(VOLSIZE)
      read_superblock(SB, ERR)
      if (ERR.get != types.error.ESUCCESS) {
        debug("persistence-io: reading superblock failed")
      }
      if (ERR.get == types.error.ESUCCESS) {
        read_log(LOG, ERR)
        if (ERR.get != types.error.ESUCCESS) {
          debug("persistence-io: reading log failed")
        }
      }
      if (ERR.get == types.error.ESUCCESS) {
        read_orphans(ORPHANS, ERR)
        if (ERR.get != types.error.ESUCCESS) {
          debug("persistence-io: reading orphans failed")
        }
      }
      if (ERR.get == types.error.ESUCCESS) {
        VOLSIZE := VOLSIZE.get - SB.main
        val BUF: buffer = new buffer()
        read_lebs(SB.lpt, lptlebs(VOLSIZE.get, LEBSIZE), BUF, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          LPT.allocate(VOLSIZE.get, types.lprops.uninit)
          LPT.fill(types.lprops.mklp(0, 0, types.lpropflags.LP_FREE, 0))
          decode_lpt(BUF, LPT, ERR)
        }
        if (ERR.get != types.error.ESUCCESS) {
          debug("persistence-io: reading LPT failed")
        }
      }
      if (ERR.get == types.error.ESUCCESS) {
        LOGOFF = LEBSIZE
        INDEXADR0 := SB.indexaddr
        MAXINO0 := SB.maxino
      }
    }
  }

  override def remap(LNUM: Int, ERR: Ref[error]): Unit = {
    ebm_avol.remap(SB.main + LNUM, ERR)
  }

  override def requires_commit(COMMIT_ : Ref[Boolean]): Unit = {
    COMMIT_ := (LOGOFF == LEBSIZE)
  }

  override def unmap(LNUM: Int): Unit = {
    ebm_avol.unmap(SB.main + LNUM)
  }

  override def write(LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    ebm_avol.write(SB.main + LNUM, OFFSET, N0, N, BUF, ERR)
  }

  def write_lebs(LNUM: Int, SIZE: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    var N: Int = 0
    while (ERR.get == types.error.ESUCCESS && N < SIZE) {
      ebm_avol.remap(LNUM + N, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        ebm_avol.write(LNUM + N, 0, N * LEBSIZE, LEBSIZE, BUF, ERR)
      }
      N = N + 1
    }
  }

  def write_orphans(LNUM: Int, ORPHANS: nat_set, ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(0).fill(0.toByte)
    encode_orphans(ORPHANS, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      ebm_avol.remap(LNUM, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      ebm_avol.write(LNUM, 0, 0, BUF.length, BUF, ERR)
    }
  }

  def write_superblock(NEWSB: superblock, ERR: Ref[error]): Unit = {
    val SIZE = Ref[Int](encoding_size(NEWSB, PAGESIZE))
    if (SIZE.get <= LEBSIZE) {
      val BUF: buffer = new buffer(SIZE.get).fill(0.toByte)
      encode_superblock(NEWSB, 0, PAGESIZE, BUF, SIZE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        ebm_avol.change(0, SIZE.get, BUF, ERR)
      }
    } else {
      ERR := types.error.ENOSPC
    }
  }

}

