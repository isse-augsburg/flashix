// Flashix: a verified file system for flash memory
// (c) 2015-2018 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import encoding.volid._
import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._
import proc._
import types._
import types.error.error
import types.wlstatus.wlstatus

class Cubi(var DoErase : Condition, var DoWl : Condition, val Eraseq : queue, var LEBSIZE : Int, var Lock : ReentrantLock, var PAGESIZE : Int, var Sqnum : Int, val VolLocks : volume_locks, val Vols : volumes, var VtblPnum : Int, val ubi_awl : UbiAwlInterface)(implicit _algebraic_implicit: algebraic.Algebraic, _procedures_implicit: proc.Procedures) extends EbmInterface {
  import _algebraic_implicit._
  import _procedures_implicit._

  def atomic_leb_change(VOLID: Byte, LNUM: Int, TO: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    val N0 = Ref[Int](0)
    datasize_h(BUF, N, N0)
    val M = Ref[Int](0)
    next_sqnum(M)
    var AVHDR: avidheader = types.avidheader.avidhdr(VOLID, LNUM, M.get, N0.get, 0)
    if (N0.get != 0) {
      AVHDR = AVHDR.updated_checksum(checksum(BUF, N0.get))
    }
    ubi_awl.write_vidhdr(TO, AVHDR, ERR)
    if (ERR.get == types.error.ESUCCESS && N0.get > 0) {
      ubi_awl.write_data_wl(TO, alignUp(N0.get, PAGESIZE), BUF, ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      if (VOLID == VTBL_VOLID && LNUM == VTBL_LNUM) {
        wl_put_peb(types.lebref.×(VTBL_VOLID, VTBL_LNUM), VtblPnum)
        VtblPnum = TO
      } else {
        if (Vols(VOLID)(LNUM) != types.ebaentry.unmapped) {
          val PNUM: Int = Vols(VOLID)(LNUM).pnum
          wl_put_peb(types.lebref.×(VOLID, LNUM), PNUM)
        }
        Vols(VOLID)(LNUM) = types.ebaentry.embed(TO)
      }
      ubi_awl.set_status(TO, types.wlstatus.used)
    } else {
      wl_put_peb(types.lebref.none, TO)
    }
  }

  override def change(VOLID: Byte, LNUM: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    VolLocks(VOLID)(LNUM).writeLock().lock()
    Lock.lock
    val TO = Ref[Int](0)
    wl_get_peb(TO, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      atomic_leb_change(VOLID, LNUM, TO.get, N, BUF, ERR)
    }
    DoErase.signal
    VolLocks(VOLID)(LNUM).writeLock().unlock()
    Lock.unlock
  }

  def erase_worker(): Unit = {
    Lock.lock
    if (Eraseq.isEmpty)
      DoErase.await
    val ERR = Ref[error](types.error.ESUCCESS)
    while (! Eraseq.isEmpty && ERR.get == types.error.ESUCCESS) {
      val EQENT: erasequeueentry = Eraseq.head
      erase_worker_helper(EQENT.pnum, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        Eraseq.removeHead
      }
    }
    val ShouldWl = Ref[Boolean](helpers.scala.Boolean.uninit)
    ubi_awl.check_wl(ShouldWl)
    if (ShouldWl.get) {
      DoWl.signal
    }
    Lock.unlock
  }

  def erase_worker_helper(PNUM: Int, ERR: Ref[error]): Unit = {
    ERR := types.error.EIO
    val N = Ref[Int](0)
    ubi_awl.get_ec(PNUM, N)
    N := N.get + 1
    ubi_awl.set_ec(PNUM, N.get)
    ubi_awl.sync_erase(PNUM, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      ubi_awl.write_echdr(PNUM, types.aecheader.aechdr(N.get), ERR)
    }
    if (ERR.get == types.error.ESUCCESS) {
      ubi_awl.set_peb_free(PNUM)
    } else {
      ubi_awl.markbad(PNUM, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        ubi_awl.set_status(PNUM, types.wlstatus.erroneous)
      }
    }
  }

  def fix_ecs(N: Int, INVALIDECS: nat_list): Unit = {
    while (! INVALIDECS.isEmpty) {
      ubi_awl.set_ec(INVALIDECS.head, N)
      INVALIDECS.removeHead
    }
  }

  override def format(VOLID: Byte, N: Int, ERR: Ref[error]): Unit = {
    ubi_awl.format(ERR)
    
    {
      val nat_variable0: Ref[Int] = Ref[Int](LEBSIZE)
      ubi_awl.get_leb_size(nat_variable0)
      LEBSIZE = nat_variable0.get
    }
    
    {
      val nat_variable0: Ref[Int] = Ref[Int](PAGESIZE)
      ubi_awl.get_page_size(nat_variable0)
      PAGESIZE = nat_variable0.get
    }
    Lock = new ReentrantLock()
    DoErase = Lock.newCondition()
    DoWl = Lock.newCondition()
    if (ERR.get != types.error.ESUCCESS) {
      debug("ubi: ubi-io format failed")
    } else {
      val N = Ref[Int](0)
      ubi_awl.get_blockcount(N)
      ERR := types.error.ESUCCESS
      var PNUM: Int = 0
      while (PNUM < N.get && ERR.get == types.error.ESUCCESS) {
        val ISBAD = Ref[Boolean](helpers.scala.Boolean.uninit)
        ubi_awl.isbad(PNUM, ISBAD, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          if (ISBAD.get != true) {
            val AEHDR = Ref[aecheader](types.aecheader.uninit)
            val BITFLIPS = Ref[Boolean](helpers.scala.Boolean.uninit)
            ubi_awl.read_echdr(PNUM, AEHDR, BITFLIPS, ERR)
            if (ERR.get == types.error.ESUCCESS && AEHDR.get.isInstanceOf[types.aecheader.aechdr]) {
              ubi_awl.set_ec(PNUM, AEHDR.get.ec)
              ubi_awl.set_peb_free(PNUM)
            }
          } else {
            ubi_awl.set_ec(PNUM, 0)
            ubi_awl.set_status(PNUM, types.wlstatus.erroneous)
          }
        }
        PNUM = PNUM + 1
      }
    }
    if (ERR.get == types.error.ESUCCESS) {
      
      {
        val nat_variable0: Ref[Int] = Ref[Int](VtblPnum)
        ubi_awl.get_free_peb(nat_variable0, ERR)
        VtblPnum = nat_variable0.get
      }
      if (ERR.get == types.error.ESUCCESS) {
        ubi_awl.write_vidhdr(VtblPnum, types.avidheader.avidhdr(VTBL_VOLID, VTBL_LNUM, 0, 0, 0), ERR)
        if (ERR.get == types.error.ESUCCESS) {
          if (ENCODED_NAT_SIZE + (ENCODED_VOLID_SIZE + ENCODED_NAT_SIZE) > LEBSIZE) {
            ERR := types.error.ENOSPC
          } else {
            val BUF: buffer = new buffer(LEBSIZE).fill(0.toByte)
            val VTBL: vtbl = new vtbl()
            VTBL(VOLID) = N
            encode_vtbl(VTBL, BUF, ERR)
            if (ERR.get == types.error.ESUCCESS) {
              ubi_awl.write_data(VtblPnum, 0, 0, LEBSIZE, BUF, ERR)
            }
          }
        }
        ubi_awl.set_status(VtblPnum, types.wlstatus.used)
        Vols.clear
        Vols(VOLID) = new ebatbl(N)
        Vols(VOLID).fill(types.ebaentry.unmapped)
        VolLocks.clear
        init_vollocks(VOLID, N)
        Eraseq.clear
      }
    }
  }

  override def get_leb_size(N: Ref[Int]): Unit = {
    N := LEBSIZE
  }

  override def get_page_size(N: Ref[Int]): Unit = {
    N := PAGESIZE
  }

  override def get_volume_size(VOLID: Byte, N: Ref[Int]): Unit = {
    N := Vols(VOLID).length
  }

  def init_vollocks(VOLID: Byte, N: Int): Unit = {
    VolLocks(VOLID) = new rwlock_array(N)
    var M: Int = 0
    while (M < N) {
      VolLocks(VOLID)(M) = new ReentrantReadWriteLock()
      M = M + 1
    }
  }

  def init_vols_mappings(RECS: recoveryentries): Unit = {
    RECS -= types.lebadress.×(VTBL_VOLID, VTBL_LNUM)
    while (! RECS.isEmpty) {
      val LADR: lebadress = RECS.headKey
      val N: Int = RECS(LADR).pnum
      if (Vols.contains(LADR.vol) && LADR.leb < Vols(LADR.vol).length) {
        Vols(LADR.vol)(LADR.leb) = types.ebaentry.embed(N)
      } else {
        ubi_awl.put_peb(N)
        Eraseq += types.erasequeueentry.eq_entry(N, types.lebref.×(LADR.vol, LADR.leb))
      }
      RECS -= LADR
    }
  }

  def init_vols_sizes(RECS: recoveryentries, VTBL: vtbl): Unit = {
    Vols.clear
    while (! VTBL.isEmpty) {
      val VOLID: Byte = VTBL.headKey
      Vols(VOLID) = new ebatbl(VTBL(VOLID))
      Vols(VOLID).fill(types.ebaentry.unmapped)
      init_vollocks(VOLID, VTBL(VOLID))
      VTBL -= VOLID
    }
  }

  override def map(VOLID: Byte, LNUM: Int, ERR: Ref[error]): Unit = {
    VolLocks(VOLID)(LNUM).writeLock().lock()
    Lock.lock
    write_once(VOLID, LNUM, ERR)
    VolLocks(VOLID)(LNUM).writeLock().unlock()
    Lock.unlock
  }

  def next_sqnum(N: Ref[Int]): Unit = {
    N := Sqnum
    Sqnum = Sqnum + 1
  }

  override def read(VOLID: Byte, LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    VolLocks(VOLID)(LNUM).readLock().lock()
    val EBAENT: ebaentry = Vols(VOLID)(LNUM)
    if (EBAENT == types.ebaentry.unmapped) {
      BUF.fill(empty, N0, N)
      ERR := types.error.ESUCCESS
    } else {
      val PNUM: Int = EBAENT.pnum
      val BITFLIPS = Ref[Boolean](helpers.scala.Boolean.uninit)
      ubi_awl.read_data(PNUM, OFFSET, N0, N, BUF, BITFLIPS, ERR)
    }
    VolLocks(VOLID)(LNUM).readLock().unlock()
  }

  override def recover(ERR: Ref[error]): Unit = {
    ubi_awl.recover()
    Vols.clear
    VolLocks.clear
    
    {
      val nat_variable0: Ref[Int] = Ref[Int](LEBSIZE)
      ubi_awl.get_leb_size(nat_variable0)
      LEBSIZE = nat_variable0.get
    }
    
    {
      val nat_variable0: Ref[Int] = Ref[Int](PAGESIZE)
      ubi_awl.get_page_size(nat_variable0)
      PAGESIZE = nat_variable0.get
    }
    Lock = new ReentrantLock()
    DoErase = Lock.newCondition()
    DoWl = Lock.newCondition()
    val RECS: recoveryentries = new recoveryentries()
    val INVALIDECS: nat_list = new nat_list()
    val VALIDCOUNT = Ref[Int](0)
    val VALIDECSUM = Ref[Int](0)
    scan_all(RECS, VALIDCOUNT, VALIDECSUM, INVALIDECS, ERR)
    if (ERR.get == types.error.ESUCCESS && ! RECS.contains(types.lebadress.×(VTBL_VOLID, VTBL_LNUM))) {
      ERR := types.error.EINVAL
    }
    if (ERR.get == types.error.ESUCCESS) {
      VtblPnum = RECS(types.lebadress.×(VTBL_VOLID, VTBL_LNUM)).pnum
      val BUF: buffer = new buffer(LEBSIZE).fill(0.toByte)
      val BITFLIPS = Ref[Boolean](helpers.scala.Boolean.uninit)
      ubi_awl.read_data(VtblPnum, 0, 0, LEBSIZE, BUF, BITFLIPS, ERR)
      if (BITFLIPS.get) {
        ubi_awl.bitflips(VtblPnum)
      }
      if (ERR.get == types.error.ESUCCESS) {
        val VTBL: vtbl = new vtbl()
        decode_vtbl(BUF, VTBL, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          init_vols_sizes(RECS, VTBL)
          init_vols_mappings(RECS)
          val N: Int = if (VALIDCOUNT.get == 0) 0 else VALIDECSUM.get / VALIDCOUNT.get
          fix_ecs(N, INVALIDECS)
        }
      }
    }
  }

  def scan_all(RECS: recoveryentries, VALIDCOUNT: Ref[Int], VALIDECSUM: Ref[Int], INVALIDECS: nat_list, ERR: Ref[error]): Unit = {
    RECS.clear
    VALIDCOUNT := 0
    VALIDECSUM := 0
    INVALIDECS.clear
    Eraseq.clear
    Sqnum = 0
    ERR := types.error.ESUCCESS
    val N = Ref[Int](0)
    ubi_awl.get_blockcount(N)
    var PNUM: Int = 0
    while (PNUM < N.get && ERR.get == types.error.ESUCCESS) {
      val ISBAD = Ref[Boolean](helpers.scala.Boolean.uninit)
      ubi_awl.isbad(PNUM, ISBAD, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (ISBAD.get) {
          ubi_awl.set_status(PNUM, types.wlstatus.erroneous)
        } else {
          val BITFLIPS = Ref[Boolean](helpers.scala.Boolean.uninit)
          val AEHDR = Ref[aecheader](types.aecheader.uninit)
          ubi_awl.read_echdr(PNUM, AEHDR, BITFLIPS, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            if (! AEHDR.get.isInstanceOf[types.aecheader.aechdr]) {
              PNUM +=: INVALIDECS
              wl_put_peb(types.lebref.none, PNUM)
            } else {
              VALIDCOUNT := VALIDCOUNT.get + 1
              VALIDECSUM := VALIDECSUM.get + AEHDR.get.ec
              ubi_awl.set_ec(PNUM, AEHDR.get.ec)
              val AVHDR = Ref[avidheader](types.avidheader.uninit)
              ubi_awl.read_vidhdr(PNUM, AVHDR, BITFLIPS, ERR)
              if (ERR.get == types.error.ESUCCESS) {
                if (AVHDR.get == types.avidheader.empty) {
                  if (BITFLIPS.get) {
                    wl_put_peb(types.lebref.none, PNUM)
                  } else {
                    ubi_awl.set_peb_free(PNUM)
                  }
                } else                 if (AVHDR.get.isInstanceOf[types.avidheader.avidhdr]) {
                  val BUF: buffer = new buffer(LEBSIZE).fill(0.toByte)
                  if (AVHDR.get.size != 0) {
                    val BITFLIPS0 = Ref[Boolean](helpers.scala.Boolean.uninit)
                    ubi_awl.read_data(PNUM, 0, 0, LEBSIZE, BUF, BITFLIPS0, ERR)
                    BITFLIPS := (BITFLIPS.get || BITFLIPS0.get)
                  }
                  if (ERR.get == types.error.ESUCCESS) {
                    val LADR: lebadress = types.lebadress.×(AVHDR.get.vol, AVHDR.get.leb)
                    var N: Int = 0
                    if (RECS.contains(LADR)) {
                      N = RECS(LADR).sqn
                    }
                    val M = Ref[Int](0)
                    datasize(BUF, M)
                    if (!  (AVHDR.get.size != 0) || AVHDR.get.size <= M.get && AVHDR.get.checksum == checksum(BUF, AVHDR.get.size)) {
                      Sqnum = max(Sqnum, AVHDR.get.sqn + 1)
                      if (AVHDR.get.sqn >= N) {
                        ubi_awl.set_status(PNUM, types.wlstatus.used)
                        if (BITFLIPS.get) {
                          ubi_awl.bitflips(PNUM)
                        }
                        if (RECS.contains(LADR)) {
                          val PNUM0: Int = RECS(LADR).pnum
                          wl_put_peb(types.lebref.×(AVHDR.get.vol, AVHDR.get.leb), PNUM0)
                        }
                        RECS(LADR) = types.recoveryentry.recovery_entry(PNUM, AVHDR.get.sqn)
                      } else {
                        wl_put_peb(types.lebref.×(AVHDR.get.vol, AVHDR.get.leb), PNUM)
                      }
                    } else {
                      wl_put_peb(types.lebref.none, PNUM)
                    }
                  }
                } else {
                  wl_put_peb(types.lebref.none, PNUM)
                }
              }
            }
          }
        }
      }
      if (ERR.get != types.error.ESUCCESS) {
        
      }
      PNUM = PNUM + 1
    }
  }

  override def sync_device(ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
  }

  override def unmap(VOLID: Byte, LNUM: Int): Unit = {
    VolLocks(VOLID)(LNUM).writeLock().lock()
    Lock.lock
    val EBAENT: ebaentry = Vols(VOLID)(LNUM)
    if (EBAENT != types.ebaentry.unmapped) {
      Vols(VOLID)(LNUM) = types.ebaentry.unmapped
      val PNUM: Int = EBAENT.pnum
      wl_put_peb(types.lebref.×(VOLID, LNUM), PNUM)
    }
    DoErase.signal
    VolLocks(VOLID)(LNUM).writeLock().unlock()
    Lock.unlock
  }

  def wear_leveling_worker(ERR: Ref[error], IsWl: Ref[Boolean]): Unit = {
    ERR := types.error.ESUCCESS
    IsWl := false
    Lock.lock
    DoWl.await
    val AVHDR = Ref[avidheader](types.avidheader.uninit)
    val FROM = Ref[Int](0)
    val VALID = Ref[Boolean](helpers.scala.Boolean.uninit)
    ubi_awl.get_leb_for_wl(FROM, AVHDR, VALID)
    Lock.unlock
    val BITFLIPS = Ref[Boolean](helpers.scala.Boolean.uninit)
    if (VALID.get && (AVHDR.get.isInstanceOf[types.avidheader.avidhdr] && (Vols.contains(AVHDR.get.vol) && AVHDR.get.leb < Vols(AVHDR.get.vol).length))) {
      VolLocks(AVHDR.get.vol)(AVHDR.get.leb).writeLock().lock()
      Lock.lock
      if (Vols(AVHDR.get.vol)(AVHDR.get.leb) == types.ebaentry.embed(FROM.get)) {
        val PNUM = Ref[Int](0)
        val TO = Ref[Int](0)
        ubi_awl.get_pebs_for_wl(TO, PNUM, VALID, IsWl)
        if (PNUM.get == FROM.get && VALID.get) {
          val BUF: buffer = new buffer(LEBSIZE).fill(0.toByte)
          ubi_awl.read_data(FROM.get, 0, 0, LEBSIZE, BUF, BITFLIPS, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            atomic_leb_change(AVHDR.get.vol, AVHDR.get.leb, TO.get, LEBSIZE, BUF, ERR)
          }
        }
      }
      VolLocks(AVHDR.get.vol)(AVHDR.get.leb).writeLock().unlock()
      Lock.unlock
    } else     if (VALID.get && (AVHDR.get.isInstanceOf[types.avidheader.avidhdr] && (AVHDR.get.vol == VTBL_VOLID && (AVHDR.get.leb == VTBL_LNUM && FROM.get == VtblPnum)))) {
      val PNUM = Ref[Int](0)
      val TO = Ref[Int](0)
      ubi_awl.get_pebs_for_wl(TO, PNUM, VALID, IsWl)
      if (PNUM.get == FROM.get && VALID.get) {
        val BUF: buffer = new buffer(LEBSIZE).fill(0.toByte)
        ubi_awl.read_data(FROM.get, 0, 0, LEBSIZE, BUF, BITFLIPS, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          atomic_leb_change(AVHDR.get.vol, AVHDR.get.leb, TO.get, LEBSIZE, BUF, ERR)
        }
      }
    } else {
      ERR := types.error.ENOSPC
      IsWl := false
    }
  }

  def wl_get_peb(PNUM: Ref[Int], ERR: Ref[error]): Unit = {
    ubi_awl.get_free_peb(PNUM, ERR)
  }

  def wl_put_peb(LREF: lebref, PNUM: Int): Unit = {
    ubi_awl.put_peb(PNUM)
    Eraseq += types.erasequeueentry.eq_entry(PNUM, LREF)
  }

  override def write(VOLID: Byte, LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    VolLocks(VOLID)(LNUM).writeLock().lock()
    val PNUM: Int = Vols(VOLID)(LNUM).pnum
    ubi_awl.write_data(PNUM, OFFSET, N0, N, BUF, ERR)
    VolLocks(VOLID)(LNUM).writeLock().unlock()
  }

  def write_once(VOLID: Byte, LNUM: Int, ERR: Ref[error]): Unit = {
    val PNUM = Ref[Int](0)
    wl_get_peb(PNUM, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val M = Ref[Int](0)
      next_sqnum(M)
      ubi_awl.write_vidhdr(PNUM.get, types.avidheader.avidhdr(VOLID, LNUM, M.get, 0, 0), ERR)
      if (ERR.get == types.error.ESUCCESS) {
        Vols(VOLID)(LNUM) = types.ebaentry.embed(PNUM.get)
        ubi_awl.set_status(PNUM.get, types.wlstatus.used)
      } else {
        wl_put_peb(types.lebref.none, PNUM.get)
      }
    }
  }

}
