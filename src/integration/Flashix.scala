package integration

import asm._
import types._
import types.error._
import helpers.scala._

class Flashix(mtd: mtd_interface)(implicit val ops: algebraic.Algebraic, val procs: proc.Procedures) {
  import ops._
  import procs._

  // Check axioms
  procs.flashix_check_axioms

  val ubiio = new ubi_io_asm(mtd)
  val ubi = new ubi_asm(new volumes(), new queue(), 0, new wlarray(), new nat_set(), 0, ubiio)
  val wbuf = new wbuf_asm(0, new wbuf_store(), ubi)
  val persistence_io = new persistence_io_asm(superblock.uninit, 0, wbuf)
  val persistence = new persistence_asm(binheap(new key_array(), 0), new nat_list(), new nat_list(), new lp_array(), persistence_io)
  val btree = new btree_asm(znode.uninit, address(0, 0, 0), persistence)
  val journal = new gjournal_asm(0, false, new nat_set(), 0, true, 0, btree) // TODO: sync option
  val aubifs = new aubifs_asm(journal)
  val vfs = new vfs_asm(new open_files(), 0, aubifs)

  def posix: posix_interface = vfs

  def percentOf(percent: Int, amount: Int) = amount * percent / 100

  def mainAreaLEBs = {
    val _total = persistence.LPT.length
    val _free = persistence.FREELIST.length
    val reserved = percentOf(10, _total)

    val total = _total - reserved
    val free = if (reserved <= _free) _free - reserved else 0
    val log = persistence_io.LOGOFF / EB_PAGE_SIZE

    (total, free, log)
  }

  def isBlockEligibleForGC(lp: lprops) = {
    lp.ref_size < LEB_SIZE - 2 * VFS_PAGE_SIZE
  }

  def computeStats = {
    var used_bytes = 0
    val (avail, free, log) = mainAreaLEBs
    val total = persistence.LPT.length

    for (i <- 0 until total) {
      val lp = persistence.LPT(i)
      lp.flags match {
        case lpropflags.LP_FREE =>
          used_bytes += 0
        case lpropflags.LP_INDEX_NODES =>
          used_bytes += LEB_SIZE
        case lpropflags.LP_GROUP_NODES =>
          if (isBlockEligibleForGC(lp))
            used_bytes += lp.ref_size
          else
            used_bytes += LEB_SIZE
      }
    }

    val total_bytes = avail * LEB_SIZE
    val free_bytes = total_bytes - used_bytes

    (total_bytes, free_bytes)
  }
}
