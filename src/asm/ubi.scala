// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import encoding.volid._
import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import proc._
import types._
import types.error.error
import types.wlstatus.wlstatus

class ubi_asm(val VOLS : volumes, val ERASEQ : queue, var SQNUM : Int, val WLARRAY : wlarray, val BFLIPSET : nat_set, var VTBLPNUM : Int, val aubi_io : aubi_io_interface)(implicit _algebraic_implicit: algebraic.Algebraic, _procedures_implicit: proc.Procedures) extends ebm_interface {
  import _algebraic_implicit._
  import _procedures_implicit._

  def decode_vtbl(BUF: buffer, VTBL: vtbl, ERR: Ref[error]): Unit = {
    VTBL.clear
    ERR := types.error.ESUCCESS
    var INDEX: Int = 0
    val SIZE = new Ref[Int](0)
    val N = new Ref[Int](0)
    decode_nat(INDEX, BUF, N, SIZE, ERR)
    INDEX = INDEX + SIZE.get
    while (N.get != 0 && ERR.get == types.error.ESUCCESS) {
      val VOLID = new Ref[Byte](0.toByte)
      decode_volid(INDEX, BUF, VOLID, SIZE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        INDEX = INDEX + SIZE.get
        val M = new Ref[Int](0)
        decode_nat(INDEX, BUF, M, SIZE, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          INDEX = INDEX + SIZE.get
          VTBL(VOLID.get) = M.get
        }
      }
      N := N.get - 1
    }
  }

  override def ebm_change(VOLID: Byte, LNUM: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    val TO = new Ref[Int](0)
    ubi_wl_get_peb(TO, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      ubi_atomic_leb_change(VOLID, LNUM, TO.get, N, BUF, ERR)
    }
    if (ERR.get != types.error.ESUCCESS) {
      debug("ubi: change to PEB " + (toStr(TO.get) + " failed"))
    }
  }

  override def ebm_create_volume(VOLID: Byte, N: Int, ERR: Ref[error]): Unit = {
    if (VOLS.contains(VOLID) || VOLID == VTBL_VOLID)
      ERR := types.error.EINVAL
    else     if (ENCODED_NAT_SIZE + (VOLS.size + 1) * (ENCODED_VOLID_SIZE + ENCODED_NAT_SIZE) > LEB_SIZE)
      ERR := types.error.ENOSPC
    else {
      ubi_wl_flush_vol(VOLID, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val PNUM = new Ref[Int](0)
        ubi_wl_get_peb(PNUM, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          val VTBL: vtbl = to_vtbl(VOLS).deepCopy
          VTBL(VOLID) = N
          val VTBL1: vtbl = to_vtbl(VOLS).deepCopy
          VTBL1(VOLID) = N
          val BUF: buffer = new buffer(LEB_SIZE).fill(0.toByte)
          encode_vtbl(VTBL1, BUF, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            ubi_atomic_leb_change(VTBL_VOLID, VTBL_LNUM, PNUM.get, LEB_SIZE, BUF, ERR)
            if (ERR.get == types.error.ESUCCESS) {
              VOLS(VOLID) = new ebatbl(N)
              VOLS(VOLID).fill(types.ebaentry.unmapped)
            }
          }
        }
      }
    }
  }

  override def ebm_erase(VOLID: Byte, LNUM: Int, ERR: Ref[error]): Unit = {
    ebm_unmap(VOLID, LNUM)
    ubi_wl_flush(VOLID, LNUM, ERR)
  }

  override def ebm_format(ERR: Ref[error]): Unit = {
    aubi_io.aubi_io_format(ERR)
    if (ERR.get != types.error.ESUCCESS) {
      debug("ubi: ubi-io format failed")
    } else {
      val N = new Ref[Int](0)
      aubi_io.aubi_io_get_blockcount(N)
      WLARRAY.allocate(N.get, types.wlentry.uninit)
      VTBLPNUM = N.get
      ERR := types.error.ESUCCESS
      var PNUM: Int = 0
      while (PNUM < N.get && ERR.get == types.error.ESUCCESS) {
        val ISBAD = new Ref[Boolean](helpers.scala.Boolean.uninit)
        aubi_io.aubi_io_isbad(PNUM, ISBAD, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          if (ISBAD.get != true) {
            val AEHDR = new Ref[aecheader](types.aecheader.uninit)
            val BITFLIPS = new Ref[Boolean](helpers.scala.Boolean.uninit)
            aubi_io.aubi_io_read_echdr(PNUM, AEHDR, BITFLIPS, ERR)
            if (ERR.get == types.error.ESUCCESS && VTBLPNUM == N.get)
              VTBLPNUM = PNUM
            
            WLARRAY(PNUM) = types.wlentry.wl_entry(AEHDR.get.ec, types.wlstatus.free)
          } else
            WLARRAY(PNUM) = types.wlentry.wl_entry(0, types.wlstatus.erroneous)
        }
        PNUM = PNUM + 1
      }
      if (ERR.get == types.error.ESUCCESS) {
        if (VTBLPNUM == N.get)
          ERR := types.error.ENOSPC
        else {
          aubi_io.aubi_io_write_vidhdr(VTBLPNUM, types.avidheader.avidhdr(VTBL_VOLID, VTBL_LNUM, 0, 0, 0), ERR)
          if (ERR.get == types.error.ESUCCESS) {
            if (ENCODED_NAT_SIZE > LEB_SIZE)
              ERR := types.error.ENOSPC
            else {
              val BUF: buffer = new buffer(LEB_SIZE).fill(0.toByte)
              val VTBL: vtbl = new vtbl()
              encode_vtbl(VTBL, BUF, ERR)
              if (ERR.get == types.error.ESUCCESS) {
                aubi_io.aubi_io_write_data(VTBLPNUM, 0, 0, LEB_SIZE, BUF, ERR)
              }
            }
          }
        }
      }
      VOLS.clear
      WLARRAY(VTBLPNUM).status = types.wlstatus.used
      ERASEQ.clear
      SQNUM = 1
      BFLIPSET.clear
    }
  }

  override def ebm_get_volume_size(VOLID: Byte, N: Ref[Int]): Unit = {
    N := VOLS(VOLID).length
  }

  override def ebm_map(VOLID: Byte, LNUM: Int, ERR: Ref[error]): Unit = {
    val BUF: buffer = new buffer(0).fill(0.toByte)
    ebm_write(VOLID, LNUM, 0, 0, 0, BUF, ERR)
  }

  override def ebm_read(VOLID: Byte, LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    val EBAENT: ebaentry = VOLS(VOLID)(LNUM)
    if (EBAENT == types.ebaentry.unmapped) {
      BUF.fill(empty, N0, N)
      ERR := types.error.ESUCCESS
    } else {
      val PNUM: Int = EBAENT.pnum
      val BITFLIPS = new Ref[Boolean](helpers.scala.Boolean.uninit)
      aubi_io.aubi_io_read_data(PNUM, OFFSET, N0, N, BUF, BITFLIPS, ERR)
      if (ERR.get == types.error.ESUCCESS && BITFLIPS.get)
        BFLIPSET += PNUM
      
    }
  }

  override def ebm_recover(ERR: Ref[error]): Unit = {
    VOLS.clear
    val RECS: recoveryentries = new recoveryentries()
    val INVALIDECS: nat_list = new nat_list()
    val VALIDCOUNT = new Ref[Int](0)
    val VALIDECSUM = new Ref[Int](0)
    ubi_scan_all(RECS, VALIDCOUNT, VALIDECSUM, INVALIDECS, ERR)
    if (! RECS.contains(types.lebadress.×(VTBL_VOLID, VTBL_LNUM)))
      ERR := types.error.EINVAL
    
    if (ERR.get == types.error.ESUCCESS) {
      VTBLPNUM = RECS(types.lebadress.×(VTBL_VOLID, VTBL_LNUM)).pnum
      val BUF: buffer = new buffer(LEB_SIZE).fill(0.toByte)
      val BITFLIPS = new Ref[Boolean](helpers.scala.Boolean.uninit)
      aubi_io.aubi_io_read_data(VTBLPNUM, 0, 0, LEB_SIZE, BUF, BITFLIPS, ERR)
      if (BITFLIPS.get)
        BFLIPSET += VTBLPNUM
      
      if (ERR.get == types.error.ESUCCESS) {
        val VTBL: vtbl = new vtbl()
        decode_vtbl(BUF, VTBL, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          ubi_init_vols_sizes(RECS, VTBL)
          ubi_init_vols_mappings(RECS)
          val N: Int = if (VALIDCOUNT.get == 0) 0 else VALIDECSUM.get / VALIDCOUNT.get
          ubi_fix_ecs(N, INVALIDECS)
        }
      }
    }
  }

  override def ebm_sync_device(ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    while (ERR.get == types.error.ESUCCESS && ! ERASEQ.isEmpty) {
      ubi_erase_worker_helper(ERASEQ.head.pnum, ERR)
      if (ERR.get == types.error.ESUCCESS)
        ERASEQ.removeHead
      
    }
  }

  override def ebm_unmap(VOLID: Byte, LNUM: Int): Unit = {
    val EBAENT: ebaentry = VOLS(VOLID)(LNUM)
    if (EBAENT != types.ebaentry.unmapped) {
      VOLS(VOLID)(LNUM) = types.ebaentry.unmapped
      val PNUM: Int = EBAENT.pnum
      BFLIPSET -= PNUM
      ubi_wl_put_peb(types.lebref.×(VOLID, LNUM), PNUM)
    }
  }

  override def ebm_write(VOLID: Byte, LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    val EBAENT: ebaentry = VOLS(VOLID)(LNUM)
    if (EBAENT == types.ebaentry.unmapped) {
      ubi_write_multiple(VOLID, LNUM, OFFSET, N0, N, BUF, ERR)
    } else     if (N != 0) {
      aubi_io.aubi_io_write_data(EBAENT.pnum, OFFSET, N0, N, BUF, ERR)
      if (ERR.get != types.error.ESUCCESS) {
        debug("ubi: failed to write to PEB " + toStr(EBAENT.pnum))
        val BUF0: buffer = new buffer(OFFSET + N).fill(0.toByte)
        val BITFLIPS = new Ref[Boolean](helpers.scala.Boolean.uninit)
        aubi_io.aubi_io_read_data(EBAENT.pnum, 0, 0, OFFSET, BUF0, BITFLIPS, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          BUF0.copy(BUF, N0, OFFSET, N)
          debug("ubi: moving data from PEB " + toStr(EBAENT.pnum))
          ebm_change(VOLID, LNUM, OFFSET + N, BUF0, ERR)
        } else {
          debug("ubi: failed to read data from PEB " + toStr(EBAENT.pnum))
        }
      }
    } else
      ERR := types.error.ESUCCESS
  }

  def encode_vtbl(VTBL: vtbl, BUF: buffer, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    var INDEX: Int = 0
    val SIZE = new Ref[Int](0)
    encode_nat(VTBL.size, INDEX, BUF, SIZE, ERR)
    INDEX = INDEX + SIZE.get
    while (ERR.get == types.error.ESUCCESS && ! VTBL.isEmpty) {
      val VOLID: Byte = VTBL.headKey
      encode_volid(VOLID, INDEX, BUF, SIZE, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        INDEX = INDEX + SIZE.get
        encode_nat(VTBL(VOLID), INDEX, BUF, SIZE, ERR)
        INDEX = INDEX + SIZE.get
      }
      VTBL -= VOLID
    }
  }

  def ubi_atomic_leb_change(VOLID: Byte, LNUM: Int, TO: Int, N0: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    var N: Int = N0
    ERR := types.error.ESUCCESS
    N = datasize(BUF, N)
    val M = new Ref[Int](0)
    ubi_next_sqnum(M)
    var AVHDR: avidheader = types.avidheader.avidhdr(VOLID, LNUM, M.get, N, 0)
    if (N != 0)
      AVHDR = AVHDR.updated_checksum(checksum(BUF, N))
    
    aubi_io.aubi_io_write_vidhdr(TO, AVHDR, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (N > 0) {
        aubi_io.aubi_io_write_data_wl(TO, alignUp(N, EB_PAGE_SIZE), BUF, ERR)
      }
    }
    if (ERR.get == types.error.ESUCCESS) {
      if (VOLID == VTBL_VOLID && LNUM == VTBL_LNUM) {
        ubi_wl_put_peb(types.lebref.×(VTBL_VOLID, VTBL_LNUM), VTBLPNUM)
        BFLIPSET -= VTBLPNUM
        VTBLPNUM = TO
      } else {
        if (VOLS(VOLID)(LNUM) != types.ebaentry.unmapped) {
          val PNUM: Int = VOLS(VOLID)(LNUM).pnum
          ubi_wl_put_peb(types.lebref.×(VOLID, LNUM), PNUM)
          BFLIPSET -= PNUM
        }
        VOLS(VOLID)(LNUM) = types.ebaentry.embed(TO)
      }
      WLARRAY(TO).status = types.wlstatus.used
    } else {
      ubi_wl_put_peb(types.lebref.none, TO)
    }
  }

  def ubi_erase_worker(): Unit = {
    if (! ERASEQ.isEmpty) {
      val EQENT: erasequeueentry = ERASEQ.head
      val ERR = new Ref[error](types.error.uninit)
      ubi_erase_worker_helper(EQENT.pnum, ERR)
      if (ERR.get == types.error.ESUCCESS)
        ERASEQ.removeHead
      
    }
  }

  def ubi_erase_worker_helper(PNUM: Int, ERR: Ref[error]): Unit = {
    ERR := types.error.EIO
    var TRIES: Int = 0
    while (ERR.get != types.error.ESUCCESS && TRIES <= UBI_ERASE_RETRIES) {
      WLARRAY(PNUM).ec = WLARRAY(PNUM).ec + 1
      aubi_io.aubi_io_sync_erase(PNUM, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        aubi_io.aubi_io_write_echdr(PNUM, types.aecheader.aechdr(WLARRAY(PNUM).ec), ERR)
      } else {
        debug("ubi: erase of PEB " + (toStr(PNUM) + " failed"))
      }
      TRIES = TRIES + 1
    }
    if (ERR.get == types.error.ESUCCESS)
      WLARRAY(PNUM).status = types.wlstatus.free
    else {
      aubi_io.aubi_io_markbad(PNUM, ERR)
      if (ERR.get == types.error.ESUCCESS)
        WLARRAY(PNUM).status = types.wlstatus.erroneous
      else {
        debug("ubi: marking PEB " + (toStr(PNUM) + " as bad failed"))
      }
    }
  }

  def ubi_fix_ecs(N: Int, INVALIDECS: nat_list): Unit = {
    while (! INVALIDECS.isEmpty) {
      WLARRAY(INVALIDECS.head).ec = N
      INVALIDECS.removeHead
    }
  }

  def ubi_get_pebs_for_wl(TO: Ref[Int], FROM: Ref[Int], VALID: Ref[Boolean]): Unit = {
    VALID := false
    val N = new Ref[Int](0)
    val FOUND = new Ref[Boolean](helpers.scala.Boolean.uninit)
    ubi_wl_get_free_min(N, FOUND)
    if (FOUND.get) {
      ubi_wl_find_free_max_below(N.get + 2 * WL_THRESHOLD, TO)
      if (! BFLIPSET.isEmpty) {
        val N: Int = BFLIPSET.head
        FROM := N
        BFLIPSET -= N
        VALID := true
      } else {
        ubi_wl_get_used_min(FROM, FOUND)
        if (FOUND.get && WLARRAY(TO.get).ec >= WLARRAY(FROM.get).ec + WL_THRESHOLD)
          VALID := true
        
      }
    }
  }

  def ubi_init_vols_mappings(RECS: recoveryentries): Unit = {
    RECS -= types.lebadress.×(VTBL_VOLID, VTBL_LNUM)
    while (! RECS.isEmpty) {
      val LADR: lebadress = RECS.headKey
      val N: Int = RECS(LADR).pnum
      if (VOLS.contains(LADR.vol) && LADR.leb < VOLS(LADR.vol).length)
        VOLS(LADR.vol)(LADR.leb) = types.ebaentry.embed(N)
      else {
        WLARRAY(N).status = types.wlstatus.erasing
        ERASEQ += types.erasequeueentry.eq_entry(N, types.lebref.×(LADR.vol, LADR.leb))
        BFLIPSET -= N
      }
      RECS -= LADR
    }
  }

  def ubi_init_vols_sizes(RECS: recoveryentries, VTBL: vtbl): Unit = {
    VOLS.clear
    while (! VTBL.isEmpty) {
      val VOLID: Byte = VTBL.headKey
      VOLS(VOLID) = new ebatbl(VTBL(VOLID))
      VOLS(VOLID).fill(types.ebaentry.unmapped)
      VTBL -= VOLID
    }
  }

  def ubi_next_sqnum(N: Ref[Int]): Unit = {
    N := SQNUM
    SQNUM = SQNUM + 1
  }

  def ubi_scan_all(RECS: recoveryentries, VALIDCOUNT: Ref[Int], VALIDECSUM: Ref[Int], INVALIDECS: nat_list, ERR: Ref[error]): Unit = {
    RECS.clear
    BFLIPSET.clear
    VALIDCOUNT := 0
    VALIDECSUM := 0
    INVALIDECS.clear
    ERASEQ.clear
    SQNUM = 0
    ERR := types.error.ESUCCESS
    val N = new Ref[Int](0)
    aubi_io.aubi_io_get_blockcount(N)
    WLARRAY.allocate(N.get, types.wlentry.uninit)
    WLARRAY.fill(types.wlentry.wl_entry(0, types.wlstatus.free))
    var PNUM: Int = 0
    while (PNUM < N.get && ERR.get == types.error.ESUCCESS) {
      val ISBAD = new Ref[Boolean](helpers.scala.Boolean.uninit)
      aubi_io.aubi_io_isbad(PNUM, ISBAD, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (ISBAD.get)
          WLARRAY(PNUM).status = types.wlstatus.erroneous
        else {
          val BITFLIPS = new Ref[Boolean](helpers.scala.Boolean.uninit)
          val AEHDR = new Ref[aecheader](types.aecheader.uninit)
          aubi_io.aubi_io_read_echdr(PNUM, AEHDR, BITFLIPS, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            if (! AEHDR.get.isInstanceOf[types.aecheader.aechdr]) {
              PNUM +=: INVALIDECS
              WLARRAY(PNUM).status = types.wlstatus.erasing
              ERASEQ += types.erasequeueentry.eq_entry(PNUM, types.lebref.none)
            } else {
              VALIDCOUNT := VALIDCOUNT.get + 1
              VALIDECSUM := VALIDECSUM.get + AEHDR.get.ec
              WLARRAY(PNUM).ec = AEHDR.get.ec
              val AVHDR = new Ref[avidheader](types.avidheader.uninit)
              aubi_io.aubi_io_read_vidhdr(PNUM, AVHDR, BITFLIPS, ERR)
              if (ERR.get == types.error.ESUCCESS) {
                if (AVHDR.get == types.avidheader.empty) {
                  if (BITFLIPS.get) {
                    WLARRAY(PNUM).status = types.wlstatus.erasing
                    ERASEQ += types.erasequeueentry.eq_entry(PNUM, types.lebref.none)
                  } else
                    WLARRAY(PNUM).status = types.wlstatus.free
                } else                 if (AVHDR.get.isInstanceOf[types.avidheader.avidhdr]) {
                  val BUF: buffer = new buffer(LEB_SIZE).fill(0.toByte)
                  if (AVHDR.get.size != 0) {
                    val BITFLIPS0 = new Ref[Boolean](helpers.scala.Boolean.uninit)
                    aubi_io.aubi_io_read_data(PNUM, 0, 0, LEB_SIZE, BUF, BITFLIPS0, ERR)
                    BITFLIPS := (BITFLIPS.get || BITFLIPS0.get)
                  }
                  if (ERR.get == types.error.ESUCCESS) {
                    var N: Int = 0
                    val LADR: lebadress = types.lebadress.×(AVHDR.get.vol, AVHDR.get.leb)
                    if (RECS.contains(LADR))
                      N = RECS(LADR).sqn
                    
                    if (!  (AVHDR.get.size != 0) || AVHDR.get.size <= datasize(BUF) && AVHDR.get.checksum == checksum(BUF, AVHDR.get.size)) {
                      SQNUM = max(SQNUM, AVHDR.get.sqn + 1)
                      if (AVHDR.get.sqn >= N) {
                        WLARRAY(PNUM).status = types.wlstatus.used
                        if (BITFLIPS.get)
                          BFLIPSET += PNUM
                        
                        if (RECS.contains(LADR)) {
                          val PNUM0: Int = RECS(LADR).pnum
                          BFLIPSET -= PNUM0
                          WLARRAY(PNUM0).status = types.wlstatus.erasing
                          ERASEQ += types.erasequeueentry.eq_entry(PNUM0, types.lebref.×(AVHDR.get.vol, AVHDR.get.leb))
                        }
                        RECS(LADR) = types.recoveryentry.recovery_entry(PNUM, AVHDR.get.sqn)
                      } else {
                        WLARRAY(PNUM).status = types.wlstatus.erasing
                        ERASEQ += types.erasequeueentry.eq_entry(PNUM, types.lebref.×(LADR.vol, LADR.leb))
                      }
                    } else {
                      WLARRAY(PNUM).status = types.wlstatus.erasing
                      ERASEQ += types.erasequeueentry.eq_entry(PNUM, types.lebref.none)
                    }
                  }
                } else {
                  WLARRAY(PNUM).status = types.wlstatus.erasing
                  ERASEQ += types.erasequeueentry.eq_entry(PNUM, types.lebref.none)
                }
              }
            }
          }
        }
      }
      PNUM = PNUM + 1
    }
  }

  def ubi_wear_leveling_worker(ERR: Ref[error]): Unit = {
    val FROM = new Ref[Int](0)
    val TO = new Ref[Int](0)
    val VALID = new Ref[Boolean](helpers.scala.Boolean.uninit)
    ubi_get_pebs_for_wl(TO, FROM, VALID)
    if (VALID.get) {
      val BITFLIPS = new Ref[Boolean](helpers.scala.Boolean.uninit)
      val AVHDR = new Ref[avidheader](types.avidheader.uninit)
      aubi_io.aubi_io_read_vidhdr(FROM.get, AVHDR, BITFLIPS, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val BUF: buffer = new buffer(LEB_SIZE).fill(0.toByte)
        aubi_io.aubi_io_read_data(FROM.get, 0, 0, LEB_SIZE, BUF, BITFLIPS, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          ubi_atomic_leb_change(AVHDR.get.vol, AVHDR.get.leb, TO.get, LEB_SIZE, BUF, ERR)
        }
        if (ERR.get != types.error.ESUCCESS) {
          debug("ubi: wear-leveling of PEB " + (toStr(FROM.get) + (" to PEB " + (toStr(TO.get) + " failed"))))
        }
        if (ERR.get == types.error.ESUCCESS) {
          ubi_erase_worker_helper(FROM.get, ERR)
          if (ERR.get == types.error.ESUCCESS)
            ERASEQ.removeLast
          
        }
      }
    } else
      ERR := types.error.ENOSPC
  }

  def ubi_wl_find_free_max_below(N: Int, PNUM: Ref[Int]): Unit = {
    var M: Int = 0
    var FOUND: Boolean = false
    while (M < WLARRAY.length) {
      if (WLARRAY(M).status == types.wlstatus.free && (WLARRAY(M).ec < N && (FOUND != true || WLARRAY(M).ec > WLARRAY(PNUM.get).ec))) {
        FOUND = true
        PNUM := M
      }
      M = M + 1
    }
  }

  def ubi_wl_flush(VOLID: Byte, LNUM: Int, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    if (! ERASEQ.isEmpty) {
      val EQENT: erasequeueentry = ERASEQ.head
      if (EQENT.lebref == types.lebref.×(VOLID, LNUM)) {
        ubi_erase_worker_helper(EQENT.pnum, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        ERASEQ.removeHead
        ubi_wl_flush(VOLID, LNUM, ERR)
        if (EQENT.lebref != types.lebref.×(VOLID, LNUM))
          EQENT +=: ERASEQ
        
      }
    }
  }

  def ubi_wl_flush_vol(VOLID: Byte, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    if (! ERASEQ.isEmpty) {
      val EQENT: erasequeueentry = ERASEQ.head
      if (EQENT.lebref != types.lebref.none && EQENT.lebref.vol == VOLID) {
        ubi_erase_worker_helper(EQENT.pnum, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        ERASEQ.removeHead
        ubi_wl_flush_vol(VOLID, ERR)
        if (EQENT.lebref == types.lebref.none || EQENT.lebref.vol != VOLID)
          EQENT +=: ERASEQ
        
      }
    }
  }

  def ubi_wl_get_free_min(N: Ref[Int], FOUND: Ref[Boolean]): Unit = {
    FOUND := false
    var M: Int = 0
    while (M < WLARRAY.length) {
      if (WLARRAY(M).status == types.wlstatus.free && (FOUND.get != true || WLARRAY(M).ec < N.get)) {
        FOUND := true
        N := WLARRAY(M).ec
      }
      M = M + 1
    }
  }

  def ubi_wl_get_peb(PNUM: Ref[Int], ERR: Ref[error]): Unit = {
    val N = new Ref[Int](0)
    val FOUND = new Ref[Boolean](helpers.scala.Boolean.uninit)
    ubi_wl_get_free_min(N, FOUND)
    if (FOUND.get != true) {
      if (! ERASEQ.isEmpty) {
        PNUM := ERASEQ.head.pnum
        ubi_erase_worker_helper(PNUM.get, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          ERASEQ.removeHead
          if (WLARRAY(PNUM.get).status != types.wlstatus.free)
            ERR := types.error.ENOSPC
          
        } else
          ERR := types.error.ENOSPC
      } else
        ERR := types.error.ENOSPC
    } else {
      ubi_wl_find_free_max_below(N.get + WL_THRESHOLD, PNUM)
      ERR := types.error.ESUCCESS
    }
  }

  def ubi_wl_get_used_min(PNUM: Ref[Int], FOUND: Ref[Boolean]): Unit = {
    FOUND := false
    var M: Int = 0
    while (M < WLARRAY.length) {
      if (WLARRAY(M).status == types.wlstatus.used && (FOUND.get != true || WLARRAY(M).ec < WLARRAY(PNUM.get).ec)) {
        FOUND := true
        PNUM := M
      }
      M = M + 1
    }
  }

  def ubi_wl_put_peb(LREF: lebref, PNUM: Int): Unit = {
    WLARRAY(PNUM).status = types.wlstatus.erasing
    ERASEQ += types.erasequeueentry.eq_entry(PNUM, LREF)
  }

  def ubi_write_multiple(VOLID: Byte, LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    ERR := types.error.EIO
    var TRIES: Int = 0
    while (ERR.get != types.error.ESUCCESS && TRIES <= UBI_WRITE_RETRIES) {
      ubi_write_once(VOLID, LNUM, OFFSET, N0, N, BUF, ERR)
      TRIES = TRIES + 1
    }
  }

  def ubi_write_once(VOLID: Byte, LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    val PNUM = new Ref[Int](0)
    ubi_wl_get_peb(PNUM, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val M = new Ref[Int](0)
      ubi_next_sqnum(M)
      aubi_io.aubi_io_write_vidhdr(PNUM.get, types.avidheader.avidhdr(VOLID, LNUM, M.get, 0, 0), ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (N != 0) {
          aubi_io.aubi_io_write_data(PNUM.get, OFFSET, N0, N, BUF, ERR)
        }
        if (ERR.get == types.error.ESUCCESS) {
          VOLS(VOLID)(LNUM) = types.ebaentry.embed(PNUM.get)
          WLARRAY(PNUM.get).status = types.wlstatus.used
        } else {
          ubi_wl_put_peb(types.lebref.×(VOLID, LNUM), PNUM.get)
        }
      } else {
        ubi_wl_put_peb(types.lebref.none, PNUM.get)
      }
    }
  }

}

