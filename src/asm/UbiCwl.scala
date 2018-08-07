// Flashix: a verified file system for flash memory
// (c) 2015-2018 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._
import types._
import types.error.error
import types.wlstatus.wlstatus

class UbiCwl(val Bflips : nat_set, val Wl : wlarray, val free_tree : FreeTreeInterface, val used_tree : UsedTreeInterface, val aubi_io : AubiIoInterface)(implicit _algebraic_implicit: algebraic.Algebraic) extends UbiAwlInterface {
  import _algebraic_implicit._

  override def bitflips(PNUM: Int): Unit = {
    Bflips += PNUM
  }

  override def format(ERR: Ref[error]): Unit = {
    used_tree.init()
    free_tree.init()
    aubi_io.format(ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val N = Ref[Int](0)
      aubi_io.get_blockcount(N)
      Wl.allocate(N.get, types.wlentry.uninit)
      Wl.fill(types.wlentry.wl_entry(0, types.wlstatus.erroneous))
      Bflips.clear
    }
  }

  override def get_blockcount(N: Ref[Int]): Unit = {
    aubi_io.get_blockcount(N)
  }

  override def get_ec(PNUM: Int, N: Ref[Int]): Unit = {
    N := Wl(PNUM).ec
  }

  override def get_free_peb(PNUM: Ref[Int], ERR: Ref[error]): Unit = {
    val IsEmpty = Ref[Boolean](helpers.scala.Boolean.uninit)
    free_tree.is_empty(IsEmpty)
    if (IsEmpty.get) {
      ERR := types.error.ENOSPC
    } else {
      val Counter = Ref[Int](0)
      free_tree.get_min(PNUM, Counter)
      ERR := types.error.ESUCCESS
    }
  }

  override def get_leb_for_wl(FROM: Ref[Int], AVHDR: Ref[avidheader], VALID: Ref[Boolean]): Unit = {
    val IsWl = Ref[Boolean](helpers.scala.Boolean.uninit)
    val TO = Ref[Int](0)
    get_pebs_for_wl(TO, FROM, VALID, IsWl)
    if (VALID.get) {
      val ERR = Ref[error](types.error.uninit)
      val BITFLIPS = Ref[Boolean](helpers.scala.Boolean.uninit)
      aubi_io.read_vidhdr(FROM.get, AVHDR, BITFLIPS, ERR)
      if (ERR.get != types.error.ESUCCESS) {
        VALID := false
      }
    }
  }

  override def get_leb_size(N: Ref[Int]): Unit = {
    aubi_io.get_leb_size(N)
  }

  override def get_page_size(N: Ref[Int]): Unit = {
    aubi_io.get_page_size(N)
  }

  override def get_pebs_for_wl(TO: Ref[Int], FROM: Ref[Int], VALID: Ref[Boolean], IsWl: Ref[Boolean]): Unit = {
    IsWl := false
    VALID := false
    val IsEmpty = Ref[Boolean](helpers.scala.Boolean.uninit)
    free_tree.is_empty(IsEmpty)
    if (IsEmpty.get != true) {
      val Counter = Ref[Int](0)
      val Index = Ref[Int](0)
      free_tree.get_min(Index, Counter)
      val ToCounter = Ref[Int](0)
      free_tree.get_max_below_threshold(Counter.get + 2 * WL_THRESHOLD, TO, ToCounter)
      if (! Bflips.isEmpty) {
        val N0: Int = Bflips.head
        FROM := N0
        Bflips -= N0
        VALID := true
        IsWl := false
      } else {
        used_tree.is_empty(IsEmpty)
        if (IsEmpty.get != true) {
          val FromCounter = Ref[Int](0)
          used_tree.get_min(FROM, FromCounter)
          if (Wl(TO.get).ec >= Wl(FROM.get).ec + WL_THRESHOLD) {
            VALID := true
          }
        }
      }
    }
  }

  override def isbad(PNUM: Int, ISBAD: Ref[Boolean], ERR: Ref[error]): Unit = {
    aubi_io.isbad(PNUM, ISBAD, ERR)
  }

  override def markbad(PNUM: Int, ERR: Ref[error]): Unit = {
    aubi_io.markbad(PNUM, ERR)
  }

  override def put_peb(PNUM: Int): Unit = {
    if (Wl(PNUM).status == types.wlstatus.used) {
      Wl(PNUM).status = types.wlstatus.erasing
      Bflips -= PNUM
      used_tree.remove(PNUM, Wl(PNUM).ec)
    } else     if (Wl(PNUM).status == types.wlstatus.free) {
      Wl(PNUM).status = types.wlstatus.erasing
      free_tree.remove(PNUM, Wl(PNUM).ec)
    } else {
      Wl(PNUM).status = types.wlstatus.erasing
    }
  }

  override def read_data(PNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, BITFLIPS: Ref[Boolean], ERR: Ref[error]): Unit = {
    aubi_io.read_data(PNUM, OFFSET, N0, N, BUF, BITFLIPS, ERR)
  }

  override def read_echdr(PNUM: Int, AEHDR: Ref[aecheader], BITFLIPS: Ref[Boolean], ERR: Ref[error]): Unit = {
    aubi_io.read_echdr(PNUM, AEHDR, BITFLIPS, ERR)
  }

  override def read_vidhdr(PNUM: Int, AVHDR: Ref[avidheader], BITFLIPS: Ref[Boolean], ERR: Ref[error]): Unit = {
    aubi_io.read_vidhdr(PNUM, AVHDR, BITFLIPS, ERR)
  }

  override def recover(): Unit = {
    used_tree.init()
    free_tree.init()
    aubi_io.recovery()
    val N = Ref[Int](0)
    aubi_io.get_blockcount(N)
    Wl.allocate(N.get, types.wlentry.uninit)
    Wl.fill(types.wlentry.wl_entry(0, types.wlstatus.erroneous))
    Bflips.clear
  }

  override def set_ec(PNUM: Int, N: Int): Unit = {
    Wl(PNUM).ec = N
  }

  override def set_peb_free(PNUM: Int): Unit = {
    Wl(PNUM).status = types.wlstatus.free
    free_tree.insert(PNUM, Wl(PNUM).ec)
  }

  override def set_status(PNUM: Int, WlStat: wlstatus): Unit = {
    if (Wl(PNUM).status == types.wlstatus.free && WlStat != types.wlstatus.free) {
      free_tree.remove(PNUM, Wl(PNUM).ec)
    }
    if (WlStat == types.wlstatus.used) {
      used_tree.insert(PNUM, Wl(PNUM).ec)
      Wl(PNUM).status = WlStat
    } else {
      Wl(PNUM).status = WlStat
    }
  }

  override def sync_erase(PNUM: Int, ERR: Ref[error]): Unit = {
    aubi_io.sync_erase(PNUM, ERR)
  }

  override def write_data(PNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    aubi_io.write_data(PNUM, OFFSET, N0, N, BUF, ERR)
  }

  override def write_data_wl(PNUM: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    aubi_io.write_data_wl(PNUM, N, BUF, ERR)
  }

  override def write_echdr(PNUM: Int, AEHDR: aecheader, ERR: Ref[error]): Unit = {
    aubi_io.write_echdr(PNUM, AEHDR, ERR)
  }

  override def write_vidhdr(PNUM: Int, AVHDR: avidheader, ERR: Ref[error]): Unit = {
    aubi_io.write_vidhdr(PNUM, AVHDR, ERR)
  }

}
