// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import encoding.echeader._
import encoding.vidheader._
import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import proc._
import types._
import types.error.error

class ubi_io_asm(val mtd : mtd_interface)(implicit _algebraic_implicit: algebraic.Algebraic, _procedures_implicit: proc.Procedures) extends aubi_io_interface {
  import _algebraic_implicit._
  import _procedures_implicit._

  override def aubi_io_format(ERR: Ref[error]): Unit = {
    mtd.mtd_init(ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val N = new Ref[Int](0)
      mtd.mtd_get_blockcount(N)
      var M: Int = 0
      while (M < N.get && ERR.get == types.error.ESUCCESS) {
        val ISBAD = new Ref[Boolean](helpers.scala.Boolean.uninit)
        mtd.mtd_isbad(M, ISBAD, ERR)
        if (ERR.get == types.error.ESUCCESS && ISBAD.get != true) {
          val AEHDR = new Ref[aecheader](types.aecheader.uninit)
          val BITFLIPS = new Ref[Boolean](helpers.scala.Boolean.uninit)
          aubi_io_read_echdr(M, AEHDR, BITFLIPS, ERR)
          val N: Int = if (ERR.get == types.error.ESUCCESS && AEHDR.get.isInstanceOf[types.aecheader.aechdr]) AEHDR.get.ec + 1 else 0
          mtd.mtd_erase(M, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            aubi_io_write_echdr(M, types.aecheader.aechdr(N), ERR)
          }
        }
        M = M + 1
      }
    }
  }

  override def aubi_io_get_blockcount(N: Ref[Int]): Unit = {
    mtd.mtd_get_blockcount(N)
  }

  override def aubi_io_isbad(PNUM: Int, ISBAD: Ref[Boolean], ERR: Ref[error]): Unit = {
    mtd.mtd_isbad(PNUM, ISBAD, ERR)
  }

  override def aubi_io_markbad(PNUM: Int, ERR: Ref[error]): Unit = {
    mtd.mtd_markbad(PNUM, ERR)
  }

  override def aubi_io_read_data(PNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, BITFLIPS: Ref[Boolean], ERR: Ref[error]): Unit = {
    ubi_io_read(PNUM, OFFSET + 2 * EB_PAGE_SIZE, N0, N, BUF, BITFLIPS, ERR)
  }

  override def aubi_io_read_echdr(PNUM: Int, AEHDR: Ref[aecheader], BITFLIPS: Ref[Boolean], ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(EB_PAGE_SIZE).fill(0.toByte)
    ubi_io_read(PNUM, 0, 0, EB_PAGE_SIZE, BUF, BITFLIPS, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (isempty(BUF))
        AEHDR := types.aecheader.empty
      else {
        val EHDR = new Ref[echeader](types.echeader.uninit)
        decode_echeader(BUF, EHDR, ERR)
        if (ERR.get == types.error.ESUCCESS)
          AEHDR := types.aecheader.aechdr(EHDR.get.ec)
        
      }
    }
  }

  override def aubi_io_read_vidhdr(PNUM: Int, AVHDR: Ref[avidheader], BITFLIPS: Ref[Boolean], ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(EB_PAGE_SIZE).fill(0.toByte)
    ubi_io_read(PNUM, EB_PAGE_SIZE, 0, EB_PAGE_SIZE, BUF, BITFLIPS, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (isempty(BUF))
        AVHDR := types.avidheader.empty
      else {
        val VHDR = new Ref[vidheader](types.vidheader.uninit)
        decode_vidheader(BUF, VHDR, ERR)
        if (ERR.get == types.error.ESUCCESS)
          AVHDR := types.avidheader.avidhdr(VHDR.get.vol, VHDR.get.leb, VHDR.get.sqn, VHDR.get.size, VHDR.get.checksum)
        
      }
    }
  }

  override def aubi_io_sync_erase(PNUM: Int, ERR: Ref[error]): Unit = {
    mtd.mtd_erase(PNUM, ERR)
  }

  override def aubi_io_write_data(PNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    mtd.mtd_write(PNUM, OFFSET + 2 * EB_PAGE_SIZE, N0, N, BUF, ERR)
  }

  override def aubi_io_write_data_wl(PNUM: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    mtd.mtd_write(PNUM, 2 * EB_PAGE_SIZE, 0, N, BUF, ERR)
  }

  override def aubi_io_write_echdr(PNUM: Int, AEHDR: aecheader, ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(EB_PAGE_SIZE).fill(0.toByte)
    val EHDR: echeader = types.echeader.echdr(AEHDR.ec)
    encode_echeader(EHDR, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      mtd.mtd_write(PNUM, 0, 0, EB_PAGE_SIZE, BUF, ERR)
    }
  }

  override def aubi_io_write_vidhdr(PNUM: Int, AVHDR: avidheader, ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(EB_PAGE_SIZE).fill(0.toByte)
    val VHDR: vidheader = types.vidheader.vidhdr(AVHDR.vol, AVHDR.leb, AVHDR.sqn, AVHDR.size, AVHDR.checksum)
    encode_vidheader(VHDR, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      mtd.mtd_write(PNUM, EB_PAGE_SIZE, 0, EB_PAGE_SIZE, BUF, ERR)
    }
  }

  private def decode_echeader(buf: buffer, ehdr: Ref[echeader], err: Ref[error]): Unit = {
    if (ENCODED_EC_HEADER_SIZE <= EB_PAGE_SIZE) {
      decode_echeader_nonempty(0, buf, ehdr, err)
    } else
      err := types.error.EINVAL
  }

  private def decode_echeader_nonempty(n: Int, buf: buffer, ehdr: Ref[echeader], err: Ref[error]): Unit = {
    if (isempty(buf, n, ENCODED_EC_HEADER_SIZE))
      err := types.error.EINVAL
    else {
      val m = new Ref[Int](0)
      decode_echeader_empty(n, buf, ehdr, m, err)
    }
  }

  private def decode_vidheader(buf: buffer, vhdr: Ref[vidheader], err: Ref[error]): Unit = {
    if (ENCODED_VIDHEADER_SIZE <= EB_PAGE_SIZE) {
      decode_vidheader_nonempty(0, buf, vhdr, err)
    } else
      err := types.error.EINVAL
  }

  private def decode_vidheader_nonempty(n: Int, buf: buffer, vhdr: Ref[vidheader], err: Ref[error]): Unit = {
    if (isempty(buf, n, ENCODED_VIDHEADER_SIZE))
      err := types.error.EINVAL
    else {
      val m = new Ref[Int](0)
      decode_vidheader_empty(n, buf, vhdr, m, err)
    }
  }

  private def encode_echeader(ehdr: echeader, buf: buffer, err: Ref[error]): Unit = {
    if (ENCODED_EC_HEADER_SIZE <= EB_PAGE_SIZE) {
      encode_echeader_nonempty(ehdr, 0, buf, err)
    } else
      err := types.error.EINVAL
  }

  private def encode_echeader_nonempty(ehdr: echeader, n: Int, buf: buffer, err: Ref[error]): Unit = {
    val m = new Ref[Int](0)
    encode_echeader_empty(ehdr, n, buf, m, err)
    if (err.get == types.error.ESUCCESS && isempty(buf, n, m.get)) {
      debug("encoding_nonempty: encoding is empty")
      err := types.error.EINVAL
    }
  }

  private def encode_vidheader(vhdr: vidheader, buf: buffer, err: Ref[error]): Unit = {
    if (ENCODED_VIDHEADER_SIZE <= EB_PAGE_SIZE) {
      encode_vidheader_nonempty(vhdr, 0, buf, err)
    } else
      err := types.error.EINVAL
  }

  private def encode_vidheader_nonempty(vhdr: vidheader, n: Int, buf: buffer, err: Ref[error]): Unit = {
    val m = new Ref[Int](0)
    encode_vidheader_empty(vhdr, n, buf, m, err)
    if (err.get == types.error.ESUCCESS && isempty(buf, n, m.get)) {
      debug("encoding_nonempty: encoding is empty")
      err := types.error.EINVAL
    }
  }

  def ubi_io_read(PNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, BITFLIPS: Ref[Boolean], ERR: Ref[error]): Unit = {
    ERR := types.error.EIO
    BITFLIPS := false
    var TRIES: Int = 0
    while (ERR.get != types.error.ESUCCESS && TRIES <= UBI_READ_RETRIES) {
      mtd.mtd_read(PNUM, OFFSET, N0, N, BUF, BITFLIPS, ERR)
      if (ERR.get != types.error.ESUCCESS) {
        debug("ubi-io: failed to read from PEB " + toStr(PNUM))
      }
      TRIES = TRIES + 1
    }
  }

}

