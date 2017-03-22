// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
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

class ubi_io_asm(var PAGESIZE : Int, val mtd_asm : mtd_asm_interface)(implicit _algebraic_implicit: algebraic.Algebraic, _procedures_implicit: proc.Procedures) extends aubi_io_interface {
  import _algebraic_implicit._
  import _procedures_implicit._

  def decode_echeader(n: Int, buf: buffer, ehdr: Ref[echeader], err: Ref[error]): Unit = {
    if (ENCODED_EC_HEADER_SIZE <= n) {
      decode_echeader_nonempty(0, buf, ehdr, err)
    } else {
      err := types.error.EINVAL
    }
  }

  def decode_echeader_nonempty(n: Int, buf: buffer, ehdr: Ref[echeader], err: Ref[error]): Unit = {
    if (isempty(buf, n, ENCODED_EC_HEADER_SIZE)) {
      err := types.error.EINVAL
    } else {
      val m = Ref[Int](0)
      decode_echeader_empty(n, buf, ehdr, m, err)
    }
  }

  def decode_vidheader(n: Int, buf: buffer, vhdr: Ref[vidheader], err: Ref[error]): Unit = {
    if (ENCODED_VIDHEADER_SIZE <= n) {
      decode_vidheader_nonempty(0, buf, vhdr, err)
    } else {
      err := types.error.EINVAL
    }
  }

  def decode_vidheader_nonempty(n: Int, buf: buffer, vhdr: Ref[vidheader], err: Ref[error]): Unit = {
    if (isempty(buf, n, ENCODED_VIDHEADER_SIZE)) {
      err := types.error.EINVAL
    } else {
      val m = Ref[Int](0)
      decode_vidheader_empty(n, buf, vhdr, m, err)
    }
  }

  def encode_echeader(n: Int, ehdr: echeader, buf: buffer, err: Ref[error]): Unit = {
    if (ENCODED_EC_HEADER_SIZE <= n) {
      encode_echeader_nonempty(ehdr, 0, buf, err)
    } else {
      err := types.error.EINVAL
    }
  }

  def encode_echeader_nonempty(ehdr: echeader, n: Int, buf: buffer, err: Ref[error]): Unit = {
    val m = Ref[Int](0)
    encode_echeader_empty(ehdr, n, buf, m, err)
    if (err.get == types.error.ESUCCESS && isempty(buf, n, m.get)) {
      debug("encoding_nonempty: encoding is empty")
      err := types.error.EINVAL
    }
  }

  def encode_vidheader(n: Int, vhdr: vidheader, buf: buffer, err: Ref[error]): Unit = {
    if (ENCODED_VIDHEADER_SIZE <= n) {
      encode_vidheader_nonempty(vhdr, 0, buf, err)
    } else {
      err := types.error.EINVAL
    }
  }

  def encode_vidheader_nonempty(vhdr: vidheader, n: Int, buf: buffer, err: Ref[error]): Unit = {
    val m = Ref[Int](0)
    encode_vidheader_empty(vhdr, n, buf, m, err)
    if (err.get == types.error.ESUCCESS && isempty(buf, n, m.get)) {
      debug("encoding_nonempty: encoding is empty")
      err := types.error.EINVAL
    }
  }

  override def format(ERR: Ref[error]): Unit = {
    mtd_asm.init(ERR)
    
    {
      val pageno: Ref[Int] = Ref[Int](PAGESIZE)
      mtd_asm.get_page_size(pageno)
      PAGESIZE = pageno.get
    }
    if (ERR.get == types.error.ESUCCESS) {
      val N = Ref[Int](0)
      mtd_asm.get_blockcount(N)
      var M: Int = 0
      while (M < N.get && ERR.get == types.error.ESUCCESS) {
        val ISBAD = Ref[Boolean](helpers.scala.Boolean.uninit)
        mtd_asm.isbad(M, ISBAD, ERR)
        if (ERR.get == types.error.ESUCCESS && ISBAD.get != true) {
          val AEHDR = Ref[aecheader](types.aecheader.uninit)
          val BITFLIPS = Ref[Boolean](helpers.scala.Boolean.uninit)
          read_echdr(M, AEHDR, BITFLIPS, ERR)
          val N: Int = if (ERR.get == types.error.ESUCCESS && AEHDR.get.isInstanceOf[types.aecheader.aechdr]) AEHDR.get.ec + 1 else 0
          mtd_asm.erase(M, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            write_echdr(M, types.aecheader.aechdr(N), ERR)
          }
        }
        M = M + 1
      }
    }
  }

  override def get_blockcount(N: Ref[Int]): Unit = {
    mtd_asm.get_blockcount(N)
  }

  override def get_leb_size(N: Ref[Int]): Unit = {
    mtd_asm.get_peb_size(N)
    N := N.get - 2 * PAGESIZE
  }

  override def get_page_size(N: Ref[Int]): Unit = {
    N := PAGESIZE
  }

  override def isbad(PNUM: Int, ISBAD: Ref[Boolean], ERR: Ref[error]): Unit = {
    mtd_asm.isbad(PNUM, ISBAD, ERR)
  }

  override def markbad(PNUM: Int, ERR: Ref[error]): Unit = {
    mtd_asm.markbad(PNUM, ERR)
  }

  def read(PNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, BITFLIPS: Ref[Boolean], ERR: Ref[error]): Unit = {
    ERR := types.error.EIO
    BITFLIPS := false
    var TRIES: Int = 0
    while (ERR.get != types.error.ESUCCESS && TRIES <= UBI_READ_RETRIES) {
      mtd_asm.read(PNUM, OFFSET, N0, N, BUF, BITFLIPS, ERR)
      if (ERR.get != types.error.ESUCCESS) {
        debug("ubi-io: failed to read from PEB " + toStr(PNUM))
      }
      TRIES = TRIES + 1
    }
  }

  override def read_data(PNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, BITFLIPS: Ref[Boolean], ERR: Ref[error]): Unit = {
    read(PNUM, OFFSET + 2 * PAGESIZE, N0, N, BUF, BITFLIPS, ERR)
  }

  override def read_echdr(PNUM: Int, AEHDR: Ref[aecheader], BITFLIPS: Ref[Boolean], ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(PAGESIZE).fill(0.toByte)
    read(PNUM, 0, 0, PAGESIZE, BUF, BITFLIPS, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (isempty(BUF)) {
        AEHDR := types.aecheader.empty
      } else {
        val EHDR = Ref[echeader](types.echeader.uninit)
        decode_echeader(PAGESIZE, BUF, EHDR, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          AEHDR := types.aecheader.aechdr(EHDR.get.ec)
        }
      }
    }
  }

  override def read_vidhdr(PNUM: Int, AVHDR: Ref[avidheader], BITFLIPS: Ref[Boolean], ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(PAGESIZE).fill(0.toByte)
    read(PNUM, PAGESIZE, 0, PAGESIZE, BUF, BITFLIPS, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (isempty(BUF)) {
        AVHDR := types.avidheader.empty
      } else {
        val VHDR = Ref[vidheader](types.vidheader.uninit)
        decode_vidheader(PAGESIZE, BUF, VHDR, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          AVHDR := types.avidheader.avidhdr(VHDR.get.vol, VHDR.get.leb, VHDR.get.sqn, VHDR.get.size, VHDR.get.checksum)
        }
      }
    }
  }

  override def recovery(): Unit = {
    
    {
      val pageno: Ref[Int] = Ref[Int](PAGESIZE)
      mtd_asm.get_page_size(pageno)
      PAGESIZE = pageno.get
    }
  }

  override def sync_erase(PNUM: Int, ERR: Ref[error]): Unit = {
    mtd_asm.erase(PNUM, ERR)
  }

  override def write_data(PNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    mtd_asm.write(PNUM, OFFSET + 2 * PAGESIZE, N0, N, BUF, ERR)
  }

  override def write_data_wl(PNUM: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    mtd_asm.write(PNUM, 2 * PAGESIZE, 0, N, BUF, ERR)
  }

  override def write_echdr(PNUM: Int, AEHDR: aecheader, ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(PAGESIZE).fill(0.toByte)
    val EHDR: echeader = types.echeader.echdr(AEHDR.ec)
    encode_echeader(PAGESIZE, EHDR, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      mtd_asm.write(PNUM, 0, 0, PAGESIZE, BUF, ERR)
    }
  }

  override def write_vidhdr(PNUM: Int, AVHDR: avidheader, ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(PAGESIZE).fill(0.toByte)
    val VHDR: vidheader = types.vidheader.vidhdr(AVHDR.vol, AVHDR.leb, AVHDR.sqn, AVHDR.size, AVHDR.checksum)
    encode_vidheader(PAGESIZE, VHDR, BUF, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      mtd_asm.write(PNUM, PAGESIZE, 0, PAGESIZE, BUF, ERR)
    }
  }

}

