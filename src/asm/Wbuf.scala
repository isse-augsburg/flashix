// Flashix: a verified file system for flash memory
// (c) 2015-2019 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._
import types._
import types.error.error

class Wbuf(var BUFLEB : bufleb, var PAGESIZE : Int, var ROFS : Boolean, val WBUF : wbuf, val apersistence_io : ApersistenceIoInterface)(implicit _algebraic_implicit: algebraic.Algebraic) extends AwbufInterface {
  import _algebraic_implicit._

  override def add_log_leb(LNUM: Int, OFFSET: Int, ERR: Ref[error]): Unit = {
    if (ROFS) {
      ERR := types.error.EROFS
    } else {
      apersistence_io.add_log_leb(LNUM, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        move_buf(LNUM, OFFSET, ERR)
      }
    }
  }

  override def commit(LPT: lp_array, PROOTADR0: address, PMAXINO0: Int, ORPHANS: nat_set, ERR: Ref[error]): Unit = {
    apersistence_io.commit(LPT, PROOTADR0, PMAXINO0, ORPHANS, ERR)
    if (ERR.get != types.error.ESUCCESS) {
      ROFS = true
    }
  }

  override def destroy_buf(ERR: Ref[error]): Unit = {
    if (ROFS) {
      ERR := types.error.EROFS
    } else {
      BUFLEB = types.bufleb.nobuffer
      ERR := types.error.ESUCCESS
    }
  }

  override def enter_readonly(): Unit = {
    ROFS = true
  }

  override def format(VOLSIZE: Int, SIZE: Int, LPT: lp_array, PROOTADR0: address, PMAXINO0: Int, ERR: Ref[error]): Unit = {
    apersistence_io.format(VOLSIZE, SIZE, LPT, PROOTADR0, PMAXINO0, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      
      {
        val nat_variable0: Ref[Int] = Ref[Int](PAGESIZE)
        apersistence_io.get_page_size(nat_variable0)
        PAGESIZE = nat_variable0.get
      }
      BUFLEB = types.bufleb.nobuffer
      WBUF.content = new buffer(PAGESIZE)
      ROFS = false
    }
  }

  override def get_buf(BUFLEB0: Ref[bufleb]): Unit = {
    BUFLEB0 := BUFLEB
  }

  override def get_leb_size(N: Ref[Int]): Unit = {
    apersistence_io.get_leb_size(N)
  }

  override def get_page_size(N: Ref[Int]): Unit = {
    N := PAGESIZE
  }

  override def is_readonly(ROFS0: Ref[Boolean]): Unit = {
    ROFS0 := ROFS
  }

  override def move_buf(LNUM: Int, OFFSET: Int, ERR: Ref[error]): Unit = {
    if (ROFS) {
      ERR := types.error.EROFS
    } else {
      ERR := types.error.ESUCCESS
      if (BUFLEB != types.bufleb.buffered(LNUM)) {
        BUFLEB = types.bufleb.buffered(LNUM)
        WBUF.offset = OFFSET
        WBUF.nbytes = 0
      }
    }
  }

  override def read_buf(LNUM: Int, OFFSET: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    apersistence_io.read(LNUM, OFFSET, 0, N, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS && (types.bufleb.buffered(LNUM) == BUFLEB && (OFFSET <= WBUF.offset + WBUF.nbytes && WBUF.offset <= OFFSET + N))) {
      val BEGINBUF: Int = if (OFFSET <= WBUF.offset) WBUF.offset - OFFSET else 0
      val BEGINWBUF: Int = if (OFFSET <= WBUF.offset) 0 else OFFSET - WBUF.offset
      BUF.copy(WBUF.content, BEGINWBUF, BEGINBUF, min(N - BEGINBUF, WBUF.nbytes - BEGINWBUF))
    }
  }

  override def recover(PROOTADR0: Ref[address], PMAXINO0: Ref[Int], ORPHANS: nat_set, LOG: nat_list, LPT: lp_array, ERR: Ref[error]): Unit = {
    apersistence_io.recover(PROOTADR0, PMAXINO0, ORPHANS, LOG, LPT, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      
      {
        val nat_variable0: Ref[Int] = Ref[Int](PAGESIZE)
        apersistence_io.get_page_size(nat_variable0)
        PAGESIZE = nat_variable0.get
      }
      BUFLEB = types.bufleb.nobuffer
      WBUF.content = new buffer(PAGESIZE)
      ROFS = false
    }
  }

  override def remap(LNUM: Int, ERR: Ref[error]): Unit = {
    apersistence_io.remap(LNUM, ERR)
  }

  override def requires_commit(COMMIT_ : Ref[Boolean]): Unit = {
    apersistence_io.requires_commit(COMMIT_)
  }

  override def unmap(LNUM: Int): Unit = {
    apersistence_io.unmap(LNUM)
  }

  override def write_buf(N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    if (ROFS) {
      ERR := types.error.EROFS
    } else     if (WBUF.nbytes + N >= PAGESIZE) {
      val N0: Int = alignDown(WBUF.nbytes + N, PAGESIZE)
      val N1: Int = N0 - WBUF.nbytes
      val BUF0: buffer = new buffer(N0).fill(0.toByte)
      BUF0.copy(WBUF.content, 0, 0, WBUF.nbytes)
      val N2: Int = N - N1
      BUF0.copy(BUF, 0, WBUF.nbytes, N1)
      apersistence_io.write(BUFLEB.leb, WBUF.offset, 0, N0, BUF0, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        WBUF.content.copy(BUF, N1, 0, N2)
        WBUF.offset = WBUF.offset + N0
        WBUF.nbytes = N2
      } else {
        ROFS = true
      }
    } else {
      WBUF.content.copy(BUF, 0, WBUF.nbytes, N)
      WBUF.nbytes = WBUF.nbytes + N
      ERR := types.error.ESUCCESS
    }
  }

}
