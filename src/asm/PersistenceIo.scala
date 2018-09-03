// Flashix: a verified file system for flash memory
// (c) 2015-2018 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import encoding.lprops._
import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._
import proc._
import types._
import types.error.error
import types.lpropflags.lpropflags

class PersistenceIo(var LEBSIZE : Int, var LOGOFF : Int, var PAGESIZE : Int, val SB : superblock, val ebm_avol : EbmAvolInterface)(implicit _algebraic_implicit: algebraic.Algebraic, _procedures_implicit: proc.Procedures) extends ApersistenceIoInterface {
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

  override def format(VOLSIZE: Int, SIZE: Int, LPT: lp_array, INDEXADR0: address, MAXINO0: Int, ERR: Ref[error]): Unit = {
    val N: Int = 5 + 2 * lptlebs(VOLSIZE, SIZE)
    ebm_avol.format(VOLSIZE + N, ERR)
    if (ERR.get != types.error.ESUCCESS) {
      debug("persistence-io: ubi format failed")
    }
    if (ERR.get == types.error.ESUCCESS) {
      
      {
        val mode: Ref[Int] = Ref[Int](LEBSIZE)
        ebm_avol.get_leb_size(mode)
        LEBSIZE = mode.get
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
        val mode: Ref[Int] = Ref[Int](PAGESIZE)
        ebm_avol.get_page_size(mode)
        PAGESIZE = mode.get
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
        val EMPTY_ = Ref[Boolean](helpers.scala.Boolean.uninit)
        val RND = Ref[ref_node](types.ref_node.uninit)
        decode_ref_node(PAGESIZE, BUF, RND, EMPTY_, ERR)
        if (ERR.get == types.error.ESUCCESS && EMPTY_.get) {
          ERR := types.error.ENOENT
        } else         if (ERR.get == types.error.ESUCCESS) {
          LOG += RND.get.leb
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
      persistence_io_decode_orphans(SB.orphsize, BUF, ORPHANS, ERR)
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
        val mode: Ref[Int] = Ref[Int](LEBSIZE)
        ebm_avol.get_leb_size(mode)
        LEBSIZE = mode.get
      }
      
      {
        val mode: Ref[Int] = Ref[Int](PAGESIZE)
        ebm_avol.get_page_size(mode)
        PAGESIZE = mode.get
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
          LPT.fill(types.lprops.mklp(0, 0, types.lpropflags.LP_FREE))
          persistence_io_decode_lpt(BUF, LPT, ERR)
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
