package integration

import asm._
import types._
import types.error._
import helpers.scala._

class Flashix(mtd: MtdInterface)(implicit val ops: algebraic.Algebraic, val procs: proc.Procedures) {
  import ops._
  import procs._

  def EB_PAGE_SIZE = {
    val pagesize = Ref.empty[Int]
    mtd.get_page_size(pagesize)
    pagesize.get
  }

  def LEB_SIZE = {
    val lebsize = Ref.empty[Int]
    mtd.get_peb_size(lebsize)
    lebsize.get - 2 * EB_PAGE_SIZE
  }

  val ubiio = new UbiIoAsm(0, mtd)
  val ubiwl = new UbiCwl(new nat_set(), new wlarray(), new FreeTreeInterface with WearLevelingTree, new UsedTreeInterface with WearLevelingTree, ubiio)
  val ubi = new Cubi(null, null, new queue(), 0, null, 0, 0, new volume_locks(), new volumes(), 0, ubiwl)
  val ebm_vol = new EbmVolCasm(0, ubi)
  val persistence_io = new PersistenceIo(0, 0, 0, superblock.uninit, ebm_vol)
  val wbuf = new Wbuf(bufleb.nobuffer, 0, false, types.wbuf.uninit, persistence_io)
  val persistence = new Persistence(new nat_list(), binheap(new key_array(), 0), new gc_array(0), 0, new nat_list(), new lp_array(), wbuf)
  val btree = new Btree(address(0, 0, 0), znode.uninit, persistence) with DebugUBIFSJournal
  // TODO: option for dosync
  val journal = new Gjournal(false, 0, new nat_set(), true, 0, btree)
  val aubifs = new Aubifs(journal)
  val tcache = new Tcache(new tcache())
  val icache = new Icache(new icache(), new mscache())
  val dcache = new Dcache(new dcache())
  val pcache = new Pcache(new pcache())
  val cache = new Cache(false, false, tcache, pcache, icache, dcache, aubifs)
  val vfs = new Vfs(0, new open_files(), cache)

  def posix: PosixInterface = vfs

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
