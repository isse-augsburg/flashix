package asm

import helpers.scala._
import types._
import types.error.error

class WBUF(val WBSTORE : wbuf_store, var VOLID : Byte, val ebm : EBM)(implicit _algebraic_implicit: algebraic.Algebraic) extends AWBUF {
  import _algebraic_implicit._

  override def awbuf_change(LNUM: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    ebm.ebm_change(VOLID, LNUM, N, BUF, ERR)
  }

  override def awbuf_create_buf(LNUM: Int, OFFSET: Int): Unit = {
    WBSTORE(LNUM) = types.wbuf.mkwbuf(mkempbuf(EB_PAGE_SIZE), OFFSET, 0)
  }

  override def awbuf_destroy_buf(LNUM: Int): Unit = {
    WBSTORE -= LNUM
  }

  override def awbuf_destroy_bufs(): Unit = {
    WBSTORE.clear
  }

  override def awbuf_format(N: Int, ERR: Ref[error]): Unit = {
    WBSTORE.clear
    VOLID = default_volid
    ebm.ebm_format(ERR)
    if (ERR.get == types.error.ESUCCESS) {
      ebm.ebm_create_volume(default_volid, N, ERR)
    }
  }

  override def awbuf_get_bufs(WBS: nat_set): Unit = {
    WBS := WBSTORE.keySetWrapper.deepCopy
  }

  override def awbuf_get_volume_size(N: Ref[Int]): Unit = {
    ebm.ebm_get_volume_size(VOLID, N)
  }

  override def awbuf_is_buffered(LNUM: Int, ISBUF: Ref[Boolean]): Unit = {
    ISBUF := WBSTORE.contains(LNUM)
  }

  override def awbuf_read(LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    ebm.ebm_read(VOLID, LNUM, OFFSET, N0, N, BUF, ERR)
  }

  override def awbuf_read_buf(LNUM: Int, OFFSET: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    if (! WBSTORE.contains(LNUM)) {
      awbuf_read(LNUM, OFFSET, 0, N, BUF, ERR)
    } else {
      ERR := types.error.ESUCCESS
      val WBUF: wbuf = WBSTORE(LNUM).deepCopy
      if (WBUF.offset + WBUF.nbytes <= OFFSET)
        BUF.fill(empty, 0, N)
      else {
        if (OFFSET + N <= WBUF.offset) {
          ebm.ebm_read(VOLID, LNUM, OFFSET, 0, N, BUF, ERR)
        } else {
          val N0: Int = if (OFFSET < WBUF.offset) WBUF.offset - OFFSET else 0
          if (N0 != 0) {
            ebm.ebm_read(VOLID, LNUM, OFFSET, 0, N0, BUF, ERR)
          }
          if (ERR.get == types.error.ESUCCESS) {
            val N1: Int = max(WBUF.offset, OFFSET) - WBUF.offset
            val N2: Int = min((OFFSET + N) - WBUF.offset, WBUF.nbytes) - N1
            BUF.copy(WBUF.content, N1, N0, N2)
            BUF.fill(empty, N0 + N2, N - (N0 + N2))
          }
        }
      }
    }
  }

  override def awbuf_remap(LNUM: Int, ERR: Ref[error]): Unit = {
    awbuf_unmap(LNUM, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      ebm.ebm_map(VOLID, LNUM, ERR)
    }
  }

  override def awbuf_unmap(LNUM: Int, ERR: Ref[error]): Unit = {
    ebm.ebm_unmap(VOLID, LNUM, ERR)
    if (ERR.get == types.error.ESUCCESS)
      WBSTORE -= LNUM
    
  }

  override def awbuf_write(LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    ebm.ebm_write(VOLID, LNUM, OFFSET, N0, N, BUF, ERR)
  }

  override def awbuf_write_buf(LNUM: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    if (! WBSTORE.contains(LNUM))
      ERR := types.error.EINVAL
    else {
      var DESTROY: Boolean = false
      val WBUF: wbuf = WBSTORE(LNUM).deepCopy
      if (WBUF.nbytes + N >= EB_PAGE_SIZE) {
        val N0: Int = EB_PAGE_SIZE - WBUF.nbytes
        WBUF.content.copy(BUF, 0, WBUF.nbytes, N0)
        ebm.ebm_write(VOLID, LNUM, WBUF.offset, 0, EB_PAGE_SIZE, WBUF.content, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          val N1: Int = alignDown(N - N0, EB_PAGE_SIZE)
          if (N1 != 0) {
            ebm.ebm_write(VOLID, LNUM, WBUF.offset + EB_PAGE_SIZE, N0, N1, BUF, ERR)
          }
          if (ERR.get == types.error.ESUCCESS) {
            val N2: Int = N - (N0 + N1)
            if (WBUF.offset + (EB_PAGE_SIZE + N1) >= LEB_SIZE)
              DESTROY = true
            else {
              WBUF.content.copy(BUF, N0 + N1, 0, N2)
              WBUF.offset = WBUF.offset + (EB_PAGE_SIZE + N1)
              WBUF.nbytes = N2
            }
          } else
            DESTROY = true
        }
      } else {
        WBUF.content.copy(BUF, 0, WBUF.nbytes, N)
        WBUF.nbytes = WBUF.nbytes + N
        ERR := types.error.ESUCCESS
      }
      WBSTORE(LNUM) = WBUF
      if (DESTROY)
        WBSTORE -= LNUM
      
    }
  }

  def wbuf_recover(ERR: Ref[error]): Unit = {
    WBSTORE.clear
    VOLID = default_volid
  }

}

object WBUF {
  def apply(WBSTORE: wbuf_store, VOLID: Byte, ebm: EBM)(implicit _algebraic_implicit: algebraic.Algebraic) = {
    new WBUF(WBSTORE, VOLID, ebm)
  }
}
