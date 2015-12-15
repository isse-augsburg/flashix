// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import types._
import types.error.error

class UBIIO(val mtd : MTD)(implicit _algebraic_implicit: algebraic.Algebraic) extends AUBIIO {
  import _algebraic_implicit._

  private def io_read(PNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, BITFLIPS: Ref[Boolean], ERR: Ref[error]): Unit = {
    ERR := types.error.EIO
    var TRIES: Int = 0
    while (ERR.get != types.error.ESUCCESS && TRIES <= UBI_READ_RETRIES) {
      mtd.mtd_read(PNUM, OFFSET, N0, N, BUF, BITFLIPS, ERR)
      TRIES = TRIES + 1
    }
  }

  override def ubi_io_format(ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val N = new Ref[Int](0)
    mtd.mtd_get_blockcount(N)
    var M: Int = 0
    while (M < N.get && ERR.get == types.error.ESUCCESS) {
      val ISBAD = new Ref[Boolean](helpers.scala.Boolean.uninit)
      mtd.mtd_isbad(M, ISBAD, ERR)
      if (ISBAD.get)
        ERR := types.error.EIO
      
      if (ERR.get == types.error.ESUCCESS) {
        mtd.mtd_erase(M, ERR)
      }
      M = M + 1
    }
  }

  override def ubi_io_get_blockcount(N: Ref[Int]): Unit = {
    mtd.mtd_get_blockcount(N)
  }

  override def ubi_io_isbad(PNUM: Int, ISBAD: Ref[Boolean], ERR: Ref[error]): Unit = {
    mtd.mtd_isbad(PNUM, ISBAD, ERR)
  }

  override def ubi_io_markbad(PNUM: Int, ERR: Ref[error]): Unit = {
    mtd.mtd_markbad(PNUM, ERR)
  }

  override def ubi_io_read_data(PNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, BITFLIPS: Ref[Boolean], ERR: Ref[error]): Unit = {
    io_read(PNUM, OFFSET + 2 * EB_PAGE_SIZE, N0, N, BUF, BITFLIPS, ERR)
  }

  override def ubi_io_read_echdr(PNUM: Int, AEHDR: Ref[aecheader], BITFLIPS: Ref[Boolean], ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(EB_PAGE_SIZE).fill(0.toByte)
    io_read(PNUM, 0, 0, EB_PAGE_SIZE, BUF, BITFLIPS, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (isempty(BUF))
        AEHDR := types.aecheader.empty
      else {
        if (is_echdr(BUF))
          AEHDR := types.aecheader.aechdr(unpack_echdr(BUF).ec)
        else
          AEHDR := types.aecheader.garbage
      }
    }
  }

  override def ubi_io_read_vidhdr(PNUM: Int, AVHDR: Ref[avidheader], BITFLIPS: Ref[Boolean], ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(EB_PAGE_SIZE).fill(0.toByte)
    io_read(PNUM, EB_PAGE_SIZE, 0, EB_PAGE_SIZE, BUF, BITFLIPS, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (isempty(BUF))
        AVHDR := types.avidheader.empty
      else {
        if (is_vidhdr(BUF)) {
          val VHDRHW: vidheader = unpack_vidhdr(BUF)
          AVHDR := types.avidheader.avidhdr(VHDRHW.vol, VHDRHW.leb, VHDRHW.sqn, VHDRHW.size, VHDRHW.checksum)
        } else
          AVHDR := types.avidheader.garbage
      }
    }
  }

  override def ubi_io_sync_erase(PNUM: Int, ERR: Ref[error]): Unit = {
    mtd.mtd_erase(PNUM, ERR)
  }

  override def ubi_io_write_data(PNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    mtd.mtd_write(PNUM, OFFSET + 2 * EB_PAGE_SIZE, N0, N, BUF, ERR)
  }

  override def ubi_io_write_data_wl(PNUM: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    mtd.mtd_write(PNUM, 2 * EB_PAGE_SIZE, 0, N, BUF, ERR)
  }

  override def ubi_io_write_echdr(PNUM: Int, AEHDR: aecheader, ERR: Ref[error]): Unit = {
    val BUF: buffer = pack_echdr(types.echeader.echdr(AEHDR.ec))
    mtd.mtd_write(PNUM, 0, 0, EB_PAGE_SIZE, BUF, ERR)
  }

  override def ubi_io_write_vidhdr(PNUM: Int, AVHDR: avidheader, ERR: Ref[error]): Unit = {
    val BUF: buffer = pack_vidhdr(types.vidheader.vidhdr(AVHDR.vol, AVHDR.leb, AVHDR.sqn, AVHDR.size, AVHDR.checksum))
    mtd.mtd_write(PNUM, EB_PAGE_SIZE, 0, EB_PAGE_SIZE, BUF, ERR)
  }

}

object UBIIO {
  def apply(mtd: MTD)(implicit _algebraic_implicit: algebraic.Algebraic) = {
    new UBIIO(mtd)
  }
}
