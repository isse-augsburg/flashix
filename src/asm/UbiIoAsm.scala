// Flashix: a verified file system for flash memory
// (c) 2015-2018 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._
import proc._
import types._
import types.error.error

class UbiIoAsm(var PAGESIZE : Int, val mtd : MtdInterface)(implicit _algebraic_implicit: algebraic.Algebraic, _procedures_implicit: proc.Procedures) extends AubiIoInterface {
  import _algebraic_implicit._
  import _procedures_implicit._

  override def format(Err: Ref[error]): Unit = {
    mtd.init(Err)
    
    {
      val nat_variable0: Ref[Int] = Ref[Int](PAGESIZE)
      mtd.get_page_size(nat_variable0)
      PAGESIZE = nat_variable0.get
    }
    if (Err.get == types.error.ESUCCESS) {
      val N = Ref[Int](0)
      mtd.get_blockcount(N)
      var M: Int = 0
      while (M < N.get && Err.get == types.error.ESUCCESS) {
        val Isbad = Ref[Boolean](helpers.scala.Boolean.uninit)
        mtd.isbad(M, Isbad, Err)
        if (Err.get == types.error.ESUCCESS && Isbad.get != true) {
          val Aehdr = Ref[aecheader](types.aecheader.uninit)
          val Bitflips = Ref[Boolean](helpers.scala.Boolean.uninit)
          read_echdr(M, Aehdr, Bitflips, Err)
          val N0: Int = if (Err.get == types.error.ESUCCESS && Aehdr.get.isInstanceOf[types.aecheader.aechdr]) Aehdr.get.ec + 1 else 0
          mtd.erase(M, Err)
          if (Err.get == types.error.ESUCCESS) {
            write_echdr(M, types.aecheader.aechdr(N0), Err)
          }
        }
        M = M + 1
      }
    }
  }

  override def get_blockcount(N: Ref[Int]): Unit = {
    mtd.get_blockcount(N)
  }

  override def get_leb_size(N: Ref[Int]): Unit = {
    mtd.get_peb_size(N)
    N := N.get - 2 * PAGESIZE
  }

  override def get_page_size(N: Ref[Int]): Unit = {
    N := PAGESIZE
  }

  override def isbad(Pnum: Int, Isbad: Ref[Boolean], Err: Ref[error]): Unit = {
    mtd.isbad(Pnum, Isbad, Err)
  }

  override def markbad(Pnum: Int, Err: Ref[error]): Unit = {
    mtd.markbad(Pnum, Err)
  }

  def read(Pnum: Int, Offset: Int, N0: Int, N: Int, Buf: buffer, Bitflips: Ref[Boolean], Err: Ref[error]): Unit = {
    Err := types.error.EIO
    Bitflips := false
    var Tries: Int = 0
    while (Err.get != types.error.ESUCCESS && Tries <= UBI_READ_RETRIES) {
      mtd.read(Pnum, Offset, N0, N, Buf, Bitflips, Err)
      if (Err.get != types.error.ESUCCESS) {
        debug("ubi-io: failed to read from PEB " + toStr(Pnum))
      }
      Tries = Tries + 1
    }
  }

  override def read_data(Pnum: Int, Offset: Int, N0: Int, N: Int, Buf: buffer, Bitflips: Ref[Boolean], Err: Ref[error]): Unit = {
    read(Pnum, Offset + 2 * PAGESIZE, N0, N, Buf, Bitflips, Err)
  }

  override def read_echdr(Pnum: Int, Aehdr: Ref[aecheader], Bitflips: Ref[Boolean], Err: Ref[error]): Unit = {
    val Buf: buffer = new buffer(PAGESIZE).fill(0.toByte)
    read(Pnum, 0, 0, PAGESIZE, Buf, Bitflips, Err)
    if (Err.get == types.error.ESUCCESS) {
      val EMPTY_ = Ref[Boolean](helpers.scala.Boolean.uninit)
      val Ehdr = Ref[echeader](types.echeader.uninit)
      decode_echeader(PAGESIZE, Buf, Ehdr, EMPTY_, Err)
      if (Err.get == types.error.ESUCCESS && EMPTY_.get) {
        Aehdr := types.aecheader.empty
      } else       if (Err.get == types.error.ESUCCESS) {
        Aehdr := types.aecheader.aechdr(Ehdr.get.ec)
      } else       if (Err.get == types.error.EINVAL) {
        Aehdr := types.aecheader.garbage
        Err := types.error.ESUCCESS
      }
    }
  }

  override def read_vidhdr(Pnum: Int, Avhdr: Ref[avidheader], Bitflips: Ref[Boolean], Err: Ref[error]): Unit = {
    val Buf: buffer = new buffer(PAGESIZE).fill(0.toByte)
    read(Pnum, PAGESIZE, 0, PAGESIZE, Buf, Bitflips, Err)
    if (Err.get == types.error.ESUCCESS) {
      val EMPTY_ = Ref[Boolean](helpers.scala.Boolean.uninit)
      val Vhdr = Ref[vidheader](types.vidheader.uninit)
      decode_vidheader(PAGESIZE, Buf, Vhdr, EMPTY_, Err)
      if (Err.get == types.error.ESUCCESS && EMPTY_.get) {
        Avhdr := types.avidheader.empty
      } else       if (Err.get == types.error.ESUCCESS) {
        Avhdr := types.avidheader.avidhdr(Vhdr.get.vol, Vhdr.get.leb, Vhdr.get.sqn, Vhdr.get.size, Vhdr.get.checksum)
      } else       if (Err.get == types.error.EINVAL) {
        Avhdr := types.avidheader.garbage
        Err := types.error.ESUCCESS
      }
    }
  }

  override def recovery(): Unit = {
    
    {
      val nat_variable0: Ref[Int] = Ref[Int](PAGESIZE)
      mtd.get_page_size(nat_variable0)
      PAGESIZE = nat_variable0.get
    }
  }

  override def sync_erase(Pnum: Int, Err: Ref[error]): Unit = {
    mtd.erase(Pnum, Err)
  }

  override def write_data(Pnum: Int, Offset: Int, N0: Int, N: Int, Buf: buffer, Err: Ref[error]): Unit = {
    mtd.write(Pnum, Offset + 2 * PAGESIZE, N0, N, Buf, Err)
  }

  override def write_data_wl(Pnum: Int, N: Int, Buf: buffer, Err: Ref[error]): Unit = {
    mtd.write(Pnum, 2 * PAGESIZE, 0, N, Buf, Err)
  }

  override def write_echdr(Pnum: Int, Aehdr: aecheader, Err: Ref[error]): Unit = {
    val Buf: buffer = new buffer(PAGESIZE).fill(0.toByte)
    val Ehdr: echeader = types.echeader.echdr(Aehdr.ec)
    encode_echeader(PAGESIZE, Ehdr, Buf, Err)
    if (Err.get == types.error.ESUCCESS) {
      mtd.write(Pnum, 0, 0, PAGESIZE, Buf, Err)
    }
  }

  override def write_vidhdr(Pnum: Int, Avhdr: avidheader, Err: Ref[error]): Unit = {
    val Buf: buffer = new buffer(PAGESIZE).fill(0.toByte)
    val Vhdr: vidheader = types.vidheader.vidhdr(Avhdr.vol, Avhdr.leb, Avhdr.sqn, Avhdr.size, Avhdr.checksum)
    encode_vidheader(PAGESIZE, Vhdr, Buf, Err)
    if (Err.get == types.error.ESUCCESS) {
      mtd.write(Pnum, PAGESIZE, 0, PAGESIZE, Buf, Err)
    }
  }

}
