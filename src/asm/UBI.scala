package asm

import helpers.scala._
import types._
import types.error.error
import types.wlstatus.wlstatus

class UBI(val BFLIPSET : nat_set, val WLARRAY : wlarray, var VTBLPNUM : Int, val EQ : queue, val VOLS : volumes, var SQNUM : Int, val aubiio : AUBIIO)(implicit _algebraic_implicit: algebraic.Algebraic) extends EBM {
  import _algebraic_implicit._

  override def ebm_change(VOLID: Byte, LNUM: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    if (! VOLS.contains(VOLID) || (! (LNUM < VOLS(VOLID).length) || (! is_aligned(N, EB_PAGE_SIZE) || ! (N <= LEB_SIZE))))
      ERR := types.error.EINVAL
    else {
      val TO = new Ref[Int](0)
      ubi_wl_get_peb(TO, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        ubi_atomic_leb_change(VOLID, LNUM, TO.get, N, BUF, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          if (VOLS(VOLID)(LNUM) != types.ebaentry.unmapped) {
            val PNUM: Int = VOLS(VOLID)(LNUM).pnum
            BFLIPSET -= PNUM
            ubi_wl_put_peb(types.lebref.×(VOLID, LNUM), PNUM)
          }
          WLARRAY(TO.get).status = types.wlstatus.used
          VOLS(VOLID)(LNUM) = types.ebaentry.embed(TO.get)
        } else {
          ubi_wl_put_peb(types.lebref.none, TO.get)
        }
      }
    }
  }

  override def ebm_create_volume(VOLID: Byte, N: Int, ERR: Ref[error]): Unit = {
    if (VOLS.contains(VOLID) || VOLID == VTBL_VOLID)
      ERR := types.error.EINVAL
    else {
      ubi_wl_flush_vol(VOLID, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        val PNUM = new Ref[Int](0)
        ubi_wl_get_peb(PNUM, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          val VTBL: vtbl = to_vtbl(VOLS).deepCopy
          VTBL(VOLID) = N
          val BUF: buffer = pack_vtbl(VTBL)
          ubi_atomic_leb_change(VTBL_VOLID, VTBL_LNUM, PNUM.get, LEB_SIZE, BUF, ERR)
          if (ERR.get != types.error.ESUCCESS) {
            ubi_wl_put_peb(types.lebref.none, PNUM.get)
          } else {
            ubi_wl_put_peb(types.lebref.×(VTBL_VOLID, VTBL_LNUM), VTBLPNUM)
            BFLIPSET -= VTBLPNUM
            VTBLPNUM = PNUM.get
            VOLS(VOLID) = new ebatbl(N)
            VOLS(VOLID).fill(types.ebaentry.unmapped)
            WLARRAY(VTBLPNUM).status = types.wlstatus.used
          }
        }
      }
    }
  }

  override def ebm_erase(VOLID: Byte, LNUM: Int, ERR: Ref[error]): Unit = {
    ebm_unmap(VOLID, LNUM, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      ubi_wl_flush(VOLID, LNUM, ERR)
    }
  }

  override def ebm_format(ERR: Ref[error]): Unit = {
    aubiio.ubi_io_format(ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val N = new Ref[Int](0)
      aubiio.ubi_io_get_blockcount(N)
      WLARRAY.allocate(N.get)
      VTBLPNUM = N.get
      ERR := types.error.ESUCCESS
      var PNUM: Int = 0
      while (PNUM < N.get && ERR.get == types.error.ESUCCESS) {
        val ISBAD = new Ref[Boolean](helpers.scala.Boolean.uninit)
        aubiio.ubi_io_isbad(PNUM, ISBAD, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          if (ISBAD.get != true) {
            aubiio.ubi_io_write_echdr(PNUM, types.aecheader.aechdr(0), ERR)
            if (ERR.get == types.error.ESUCCESS && VTBLPNUM == N.get)
              VTBLPNUM = PNUM
            
            WLARRAY(PNUM) = types.wlentry.wl_entry(0, types.wlstatus.free)
          } else
            WLARRAY(PNUM) = types.wlentry.wl_entry(0, types.wlstatus.erroneous)
        }
        PNUM = PNUM + 1
      }
      if (ERR.get == types.error.ESUCCESS) {
        if (VTBLPNUM == N.get)
          ERR := types.error.ENOSPC
        else {
          aubiio.ubi_io_write_vidhdr(VTBLPNUM, types.avidheader.avidhdr(VTBL_VOLID, VTBL_LNUM, 0, 0, 0), ERR)
          if (ERR.get == types.error.ESUCCESS) {
            val BUF: buffer = pack_vtbl(new vtbl())
            aubiio.ubi_io_write_data(VTBLPNUM, 0, 0, LEB_SIZE, BUF, ERR)
          }
        }
      }
    }
    VOLS.clear
    WLARRAY(VTBLPNUM) = types.wlentry.wl_entry(0, types.wlstatus.used)
    EQ.clear
    SQNUM = 1
    BFLIPSET.clear
  }

  override def ebm_get_volume_size(VOLID: Byte, N: Ref[Int]): Unit = {
    if (VOLS.contains(VOLID))
      N := VOLS(VOLID).length
    
  }

  override def ebm_map(VOLID: Byte, LNUM: Int, ERR: Ref[error]): Unit = {
    if (! VOLS.contains(VOLID) || (! (LNUM < VOLS(VOLID).length) || VOLS(VOLID)(LNUM) != types.ebaentry.unmapped))
      ERR := types.error.EINVAL
    else {
      val BUF: buffer = new buffer(0).fill(0.toByte)
      ebm_write(VOLID, LNUM, 0, 0, 0, BUF, ERR)
    }
  }

  override def ebm_read(VOLID: Byte, LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    if (! VOLS.contains(VOLID) || (! (LNUM < VOLS(VOLID).length) || ! (OFFSET + N <= LEB_SIZE)))
      ERR := types.error.EINVAL
    else {
      ERR := types.error.ESUCCESS
      val EBAENT: ebaentry = VOLS(VOLID)(LNUM)
      if (EBAENT == types.ebaentry.unmapped)
        BUF.fill(empty, N0, N)
      else {
        val PNUM: Int = EBAENT.pnum
        val BITFLIPS = new Ref[Boolean](helpers.scala.Boolean.uninit)
        aubiio.ubi_io_read_data(PNUM, OFFSET, N0, N, BUF, BITFLIPS, ERR)
        if (ERR.get == types.error.ESUCCESS && BITFLIPS.get)
          BFLIPSET += PNUM
        
      }
    }
  }

  override def ebm_unmap(VOLID: Byte, LNUM: Int, ERR: Ref[error]): Unit = {
    if (! VOLS.contains(VOLID) || ! (LNUM < VOLS(VOLID).length))
      ERR := types.error.EINVAL
    else {
      ERR := types.error.ESUCCESS
      val EBAENT: ebaentry = VOLS(VOLID)(LNUM)
      if (EBAENT != types.ebaentry.unmapped) {
        VOLS(VOLID)(LNUM) = types.ebaentry.unmapped
        val PNUM: Int = EBAENT.pnum
        BFLIPSET -= PNUM
        ubi_wl_put_peb(types.lebref.×(VOLID, LNUM), PNUM)
      }
    }
  }

  override def ebm_write(VOLID: Byte, LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    if (! VOLS.contains(VOLID) || (! (LNUM < VOLS(VOLID).length) || (! is_aligned(N, EB_PAGE_SIZE) || (! is_aligned(OFFSET, EB_PAGE_SIZE) || ! (OFFSET + N <= LEB_SIZE)))))
      ERR := types.error.EINVAL
    else {
      val EBAENT: ebaentry = VOLS(VOLID)(LNUM)
      if (EBAENT == types.ebaentry.unmapped) {
        ubi_write_multiple(VOLID, LNUM, OFFSET, N0, N, BUF, ERR)
      } else {
        if (N != 0) {
          aubiio.ubi_io_write_data(EBAENT.pnum, OFFSET, N0, N, BUF, ERR)
          if (ERR.get != types.error.ESUCCESS) {
            val BUF0: buffer = new buffer(OFFSET + N).fill(0.toByte)
            val BITFLIPS = new Ref[Boolean](helpers.scala.Boolean.uninit)
            aubiio.ubi_io_read_data(EBAENT.pnum, 0, 0, OFFSET, BUF0, BITFLIPS, ERR)
            if (ERR.get == types.error.ESUCCESS) {
              BUF0.copy(BUF, N0, OFFSET, N)
              ebm_change(VOLID, LNUM, OFFSET + N, BUF0, ERR)
            }
          }
        } else
          ERR := types.error.ESUCCESS
      }
    }
  }

  private def erase_worker(): Unit = {
    if (! EQ.isEmpty) {
      val EQENT: erasequeueentry = EQ.head
      val ERR = new Ref[error](error.uninit)
      erase_worker_helper(EQENT.pnum, ERR)
      if (ERR.get == types.error.ESUCCESS)
        EQ.removeHead
      
    }
  }

  private def erase_worker_helper(PNUM: Int, ERR: Ref[error]): Unit = {
    ERR := types.error.EIO
    var TRIES: Int = 0
    while (ERR.get != types.error.ESUCCESS && TRIES <= UBI_ERASE_RETRIES) {
      WLARRAY(PNUM).ec = WLARRAY(PNUM).ec + 1
      aubiio.ubi_io_sync_erase(PNUM, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        aubiio.ubi_io_write_echdr(PNUM, types.aecheader.aechdr(WLARRAY(PNUM).ec), ERR)
      }
      TRIES = TRIES + 1
    }
    if (ERR.get == types.error.ESUCCESS)
      WLARRAY(PNUM).status = types.wlstatus.free
    else {
      aubiio.ubi_io_markbad(PNUM, ERR)
      if (ERR.get == types.error.ESUCCESS)
        WLARRAY(PNUM).status = types.wlstatus.erroneous
      
    }
  }

  private def fix_ecs(N: Int, INVALIDECS: nat_list): Unit = {
    while (! INVALIDECS.isEmpty) {
      WLARRAY(INVALIDECS.head).ec = N
      INVALIDECS.removeHead
    }
  }

  private def get_free_peb_for_wl(FREEECS: nat_set, PNUM: Ref[Int], ERR: Ref[error]): Unit = {
    if (FREEECS.isEmpty)
      ERR := types.error.ENOSPC
    else {
      ChooseIndex((WLARRAY), (N : Int) => (WLARRAY(N) == types.wlentry.wl_entry(max(below(FREEECS, min(FREEECS) + 2 * WL_THRESHOLD)), types.wlstatus.free)), (N : Int) =>
      {
        PNUM := N
        ERR := types.error.ESUCCESS
      })
    }
  }

  private def get_used_peb_for_wl(USEDECS: nat_set, PNUM: Ref[Int], ERR: Ref[error]): Unit = {
    if (USEDECS.isEmpty)
      ERR := types.error.ENOSPC
    else {
      ChooseIndex((WLARRAY), (N : Int) => (WLARRAY(N) == types.wlentry.wl_entry(min(USEDECS), types.wlstatus.used)), (N : Int) =>
      {
        PNUM := N
        ERR := types.error.ESUCCESS
      })
    }
  }

  private def init_vols_mappings(RECS: recoveryentries): Unit = {
    RECS -= types.lebadress.×(VTBL_VOLID, VTBL_LNUM)
    while (! RECS.isEmpty) {
      ChooseIn((RECS).keys, (LEBADR : lebadress) =>
      {
        val N: Int = RECS(LEBADR).pnum
        if (VOLS.contains(LEBADR.vol) && LEBADR.leb < VOLS(LEBADR.vol).length)
          VOLS(LEBADR.vol)(LEBADR.leb) = types.ebaentry.embed(N)
        else {
          WLARRAY(N).status = types.wlstatus.erasing
          EQ += types.erasequeueentry.eq_entry(N, types.lebref.×(LEBADR.vol, LEBADR.leb))
          BFLIPSET -= N
        }
        RECS -= LEBADR
      })
    }
  }

  private def init_vols_sizes(VTBL: vtbl): Unit = {
    VOLS.clear
    while (! VTBL.isEmpty) {
      ChooseIn((VTBL).keys, (VOLID : Byte) =>
      {
        VOLS(VOLID) = new ebatbl(VTBL(VOLID))
        VOLS(VOLID).fill(types.ebaentry.unmapped)
        VTBL -= VOLID
      })
    }
  }

  private def scan_all(RECS: recoveryentries, VALIDCOUNT: Ref[Int], VALIDECSUM: Ref[Int], INVALIDECS: nat_list, ERR: Ref[error]): Unit = {
    RECS.clear
    BFLIPSET.clear
    VALIDCOUNT := 0
    VALIDECSUM := 0
    INVALIDECS.clear
    EQ.clear
    SQNUM = 0
    ERR := types.error.ESUCCESS
    val N = new Ref[Int](0)
    aubiio.ubi_io_get_blockcount(N)
    WLARRAY.allocate(N.get)
    WLARRAY.fill(types.wlentry.wl_entry(0, types.wlstatus.free))
    var PNUM: Int = 0
    while (PNUM < N.get && ERR.get == types.error.ESUCCESS) {
      val ISBAD = new Ref[Boolean](helpers.scala.Boolean.uninit)
      aubiio.ubi_io_isbad(PNUM, ISBAD, ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (ISBAD.get)
          WLARRAY(PNUM).status = types.wlstatus.erroneous
        else {
          val BITFLIPS = new Ref[Boolean](helpers.scala.Boolean.uninit)
          val AEHDR = new Ref[aecheader](aecheader.uninit)
          aubiio.ubi_io_read_echdr(PNUM, AEHDR, BITFLIPS, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            if (! AEHDR.get.isInstanceOf[types.aecheader.aechdr]) {
              PNUM +=: INVALIDECS
              WLARRAY(PNUM).status = types.wlstatus.erasing
              EQ += types.erasequeueentry.eq_entry(PNUM, types.lebref.none)
            } else {
              VALIDCOUNT := VALIDCOUNT.get + 1
              VALIDECSUM := VALIDECSUM.get + AEHDR.get.ec
              WLARRAY(PNUM).ec = AEHDR.get.ec
              val AVHDR = new Ref[avidheader](avidheader.uninit)
              aubiio.ubi_io_read_vidhdr(PNUM, AVHDR, BITFLIPS, ERR)
              if (ERR.get == types.error.ESUCCESS) {
                if (AVHDR.get == types.avidheader.empty) {
                  if (BITFLIPS.get) {
                    WLARRAY(PNUM).status = types.wlstatus.erasing
                    EQ += types.erasequeueentry.eq_entry(PNUM, types.lebref.none)
                  } else
                    WLARRAY(PNUM).status = types.wlstatus.free
                } else {
                  if (AVHDR.get.isInstanceOf[types.avidheader.avidhdr]) {
                    val BUF: buffer = new buffer(LEB_SIZE).fill(0.toByte)
                    if (AVHDR.get.size != 0) {
                      val BITFLIPS0 = new Ref[Boolean](helpers.scala.Boolean.uninit)
                      aubiio.ubi_io_read_data(PNUM, 0, 0, LEB_SIZE, BUF, BITFLIPS0, ERR)
                      BITFLIPS := (BITFLIPS.get || BITFLIPS0.get)
                    }
                    if (ERR.get == types.error.ESUCCESS) {
                      var N: Int = 0
                      val LEBADR: lebadress = types.lebadress.×(AVHDR.get.vol, AVHDR.get.leb)
                      if (RECS.contains(LEBADR))
                        N = RECS(LEBADR).sqn
                      
                      if (!  (AVHDR.get.size != 0) || AVHDR.get.size <= datasize(BUF) && AVHDR.get.checksum == checksum(BUF, AVHDR.get.size)) {
                        SQNUM = max(SQNUM, AVHDR.get.sqn + 1)
                        if (AVHDR.get.sqn >= N) {
                          WLARRAY(PNUM).status = types.wlstatus.used
                          if (BITFLIPS.get)
                            BFLIPSET += PNUM
                          
                          if (RECS.contains(LEBADR)) {
                            val PNUM0: Int = RECS(LEBADR).pnum
                            BFLIPSET -= PNUM0
                            WLARRAY(PNUM0).status = types.wlstatus.erasing
                            EQ += types.erasequeueentry.eq_entry(PNUM0, types.lebref.×(AVHDR.get.vol, AVHDR.get.leb))
                          }
                          RECS(LEBADR) = types.recoveryentry.recovery_entry(PNUM, AVHDR.get.sqn)
                        } else {
                          WLARRAY(PNUM).status = types.wlstatus.erasing
                          EQ += types.erasequeueentry.eq_entry(PNUM, types.lebref.×(LEBADR.vol, LEBADR.leb))
                        }
                      } else {
                        WLARRAY(PNUM).status = types.wlstatus.erasing
                        EQ += types.erasequeueentry.eq_entry(PNUM, types.lebref.none)
                      }
                    }
                  } else {
                    WLARRAY(PNUM).status = types.wlstatus.erasing
                    EQ += types.erasequeueentry.eq_entry(PNUM, types.lebref.none)
                  }
                }
              }
            }
          }
        }
      }
      PNUM = PNUM + 1
    }
  }

  private def ubi_atomic_leb_change(VOLID: Byte, LNUM: Int, TO: Int, N0: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    var N = N0
    ERR := types.error.ESUCCESS
    N = datasize(BUF, N)
    val M = new Ref[Int](0)
    ubi_next_sqnum(M)
    var AVHDR: avidheader = types.avidheader.avidhdr(VOLID, LNUM, M.get, N, 0)
    if (N != 0)
      AVHDR = AVHDR.updated_checksum(checksum(BUF, N))
    
    aubiio.ubi_io_write_vidhdr(TO, AVHDR, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      if (N > 0) {
        aubiio.ubi_io_write_data_wl(TO, alignUp(N, EB_PAGE_SIZE), BUF, ERR)
      }
    }
  }

  private def ubi_next_sqnum(N: Ref[Int]): Unit = {
    N := SQNUM
    SQNUM = SQNUM + 1
  }

  def ubi_recover(ERR: Ref[error]): Unit = {
    VOLS.clear
    VTBLPNUM = 0
    BFLIPSET.clear
    val RECS: recoveryentries = new recoveryentries()
    val INVALIDECS: nat_list = new nat_list()
    val VALIDCOUNT = new Ref[Int](0)
    val VALIDECSUM = new Ref[Int](0)
    scan_all(RECS, VALIDCOUNT, VALIDECSUM, INVALIDECS, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val N: Int = if (VALIDCOUNT.get == 0) 0 else VALIDECSUM.get / VALIDCOUNT.get
      fix_ecs(N, INVALIDECS)
    }
    if (! RECS.contains(types.lebadress.×(VTBL_VOLID, VTBL_LNUM)))
      ERR := types.error.EINVAL
    
    if (ERR.get == types.error.ESUCCESS) {
      VTBLPNUM = RECS(types.lebadress.×(VTBL_VOLID, VTBL_LNUM)).pnum
      val BUF: buffer = new buffer(LEB_SIZE).fill(0.toByte)
      val BITFLIPS = new Ref[Boolean](helpers.scala.Boolean.uninit)
      aubiio.ubi_io_read_data(VTBLPNUM, 0, 0, LEB_SIZE, BUF, BITFLIPS, ERR)
      if (BITFLIPS.get)
        BFLIPSET += VTBLPNUM
      
      if (ERR.get == types.error.ESUCCESS) {
        val VTBL: vtbl = unpack_vtbl(BUF)
        init_vols_sizes(VTBL)
        init_vols_mappings(RECS)
      }
    }
  }

  private def ubi_wl_flush(VOLID: Byte, LNUM: Int, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    if (! EQ.isEmpty) {
      val EQENT: erasequeueentry = EQ.head
      if (EQENT.lebref == types.lebref.×(VOLID, LNUM)) {
        erase_worker_helper(EQENT.pnum, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        EQ.removeHead
        ubi_wl_flush(VOLID, LNUM, ERR)
        if (EQENT.lebref != types.lebref.×(VOLID, LNUM))
          EQENT +=: EQ
        
      }
    }
  }

  private def ubi_wl_flush_vol(VOLID: Byte, ERR: Ref[error]): Unit = {
    ERR := types.error.ESUCCESS
    if (! EQ.isEmpty) {
      val EQENT: erasequeueentry = EQ.head
      if (EQENT.lebref != types.lebref.none && EQENT.lebref.vol == VOLID) {
        erase_worker_helper(EQENT.pnum, ERR)
      }
      if (ERR.get == types.error.ESUCCESS) {
        EQ.removeHead
        ubi_wl_flush_vol(VOLID, ERR)
        if (EQENT.lebref == types.lebref.none || EQENT.lebref.vol != VOLID)
          EQENT +=: EQ
        
      }
    }
  }

  private def ubi_wl_get_peb(PNUM: Ref[Int], ERR: Ref[error]): Unit = {
    val FREEECS: nat_set = free_ecs(WLARRAY).deepCopy
    if (FREEECS.isEmpty) {
      if (! EQ.isEmpty) {
        PNUM := EQ.head.pnum
        erase_worker_helper(PNUM.get, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          EQ.removeHead
          if (WLARRAY(PNUM.get).status != types.wlstatus.free)
            ERR := types.error.ENOSPC
          
        } else
          ERR := types.error.ENOSPC
      } else
        ERR := types.error.ENOSPC
    } else {
      ChooseIndex((WLARRAY), (N : Int) => (WLARRAY(N) == types.wlentry.wl_entry(max(below(FREEECS, min(FREEECS) + WL_THRESHOLD)), types.wlstatus.free)), (N : Int) =>
      {
        PNUM := N
        ERR := types.error.ESUCCESS
      })
    }
  }

  private def ubi_wl_put_peb(LREF: lebref, PNUM: Int): Unit = {
    WLARRAY(PNUM).status = types.wlstatus.erasing
    EQ += types.erasequeueentry.eq_entry(PNUM, LREF)
  }

  private def ubi_write_multiple(VOLID: Byte, LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    ERR := types.error.EIO
    var TRIES: Int = 0
    while (ERR.get != types.error.ESUCCESS && TRIES <= UBI_WRITE_RETRIES) {
      ubi_write_once(VOLID, LNUM, OFFSET, N0, N, BUF, ERR)
      TRIES = TRIES + 1
    }
  }

  private def ubi_write_once(VOLID: Byte, LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    val PNUM = new Ref[Int](0)
    ubi_wl_get_peb(PNUM, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      val M = new Ref[Int](0)
      ubi_next_sqnum(M)
      aubiio.ubi_io_write_vidhdr(PNUM.get, types.avidheader.avidhdr(VOLID, LNUM, M.get, 0, 0), ERR)
      if (ERR.get == types.error.ESUCCESS) {
        if (N != 0) {
          aubiio.ubi_io_write_data(PNUM.get, OFFSET, N0, N, BUF, ERR)
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

  private def wear_leveling_worker(ERR: Ref[error]): Unit = {
    val FREEECS: nat_set = free_ecs(WLARRAY).deepCopy
    if (FREEECS.isEmpty)
      ERR := types.error.ENOSPC
    else {
      var VALID: Boolean = false
      val FROM = new Ref[Int](0)
      val TO = new Ref[Int](0)
      if (! BFLIPSET.isEmpty) {
        ChooseIn((BFLIPSET).set, (N : Int) =>
        {
          get_free_peb_for_wl(FREEECS, TO, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            FROM := N
            BFLIPSET -= N
            VALID = true
          }
        })
      }
      if (VALID != true) {
        val USEDECS: nat_set = used_ecs(WLARRAY).deepCopy
        if (! USEDECS.isEmpty) {
          get_free_peb_for_wl(FREEECS, TO, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            get_used_peb_for_wl(USEDECS, FROM, ERR)
          }
          if (ERR.get == types.error.ESUCCESS && WLARRAY(TO.get).ec >= WLARRAY(FROM.get).ec + WL_THRESHOLD)
            VALID = true
          
        }
      }
      if (VALID) {
        val BITFLIPS = new Ref[Boolean](helpers.scala.Boolean.uninit)
        val AVHDR = new Ref[avidheader](avidheader.uninit)
        aubiio.ubi_io_read_vidhdr(FROM.get, AVHDR, BITFLIPS, ERR)
        if (ERR.get == types.error.ESUCCESS) {
          val BUF: buffer = new buffer(LEB_SIZE).fill(0.toByte)
          aubiio.ubi_io_read_data(FROM.get, 0, 0, LEB_SIZE, BUF, BITFLIPS, ERR)
          if (ERR.get == types.error.ESUCCESS) {
            ubi_atomic_leb_change(AVHDR.get.vol, AVHDR.get.leb, TO.get, LEB_SIZE, BUF, ERR)
          }
          if (ERR.get == types.error.ESUCCESS) {
            if (AVHDR.get.vol == VTBL_VOLID && AVHDR.get.leb == VTBL_LNUM)
              VTBLPNUM = TO.get
            else
              VOLS(AVHDR.get.vol)(AVHDR.get.leb) = types.ebaentry.embed(TO.get)
            WLARRAY(TO.get).status = types.wlstatus.used
            ubi_wl_put_peb(types.lebref.×(AVHDR.get.vol, AVHDR.get.leb), FROM.get)
            erase_worker_helper(FROM.get, ERR)
            if (ERR.get == types.error.ESUCCESS)
              EQ.removeLast
            
          } else {
            ubi_wl_put_peb(types.lebref.none, TO.get)
          }
        }
      } else
        ERR := types.error.ENOSPC
    }
  }

}

object UBI {
  def apply(BFLIPSET: nat_set, WLARRAY: wlarray, VTBLPNUM: Int, EQ: queue, VOLS: volumes, SQNUM: Int, aubiio: AUBIIO)(implicit _algebraic_implicit: algebraic.Algebraic) = {
    new UBI(BFLIPSET, WLARRAY, VTBLPNUM, EQ, VOLS, SQNUM, aubiio)
  }
}
