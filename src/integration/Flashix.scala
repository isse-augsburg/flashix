package integration

import asm._
import types._
import types.error._
import helpers.scala._
import visualization.Observable

class Flashix(cachingStrategy: Flashix.CachingStrategy, concurrencyStrategy: Flashix.ConcurrencyStrategy, concurrentUpdate: Flashix => Unit, mtd: MtdInterface)
             (implicit val ops: algebraic.Algebraic, val procs: proc.Procedures) {
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
  val journal = new Gjournal(cachingStrategy != Flashix.NoCaching, 0, new nat_set(), true, 0, btree)
  val aubifs = new Aubifs(journal)
  val cachedAubifs = {
    if (cachingStrategy == Flashix.AfsCaching) {
      val tcache = new Tcache(new tcache())
      val icache = new Icache(new icache(), new mscache())
      val dcache = new Dcache(new dcache())
      val pcache = new Pcache(new pcache())
      new Cache(false, false, tcache, pcache, icache, dcache, aubifs)
    } else
      aubifs
  }
  val vfs = new Vfs(0, new open_files(), cachedAubifs)

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

  private var wearleveling: Thread = _
  private var erase: Thread = _

  def startConcurrentOps {
    val flashix = this
    // Start concurrent erase/wear-leveling
    wearleveling = new Thread {
      override def run {
        try {
          while (!this.isInterrupted()) {
            println("ubi: waiting for wear-leveling")
            val err: Ref[error] = Ref(error.ESUCCESS)
            val iswl: Ref[Boolean] = Ref(false)
            ubi.wear_leveling_worker(err, iswl)
            println("ubi: performed wear-leveling, err = " + err.get)

            concurrentUpdate(flashix)
          }
        } catch {
          case _: InterruptedException =>
            println("ubi: wear-leveling thread interrupted")
        }
      }
    }
    erase = new Thread {
      override def run {
        try {
          while (!this.isInterrupted()) {
            println("ubi: waiting for erase")
            ubi.erase_worker
            println("ubi: performed erasing")

            concurrentUpdate(flashix)
          }
        } catch {
          case _: InterruptedException =>
            println("ubi: erase thread interrupted")
        }
      }
    }
    wearleveling.start
    erase.start
  }

  def joinConcurrentOps {
    /* TODO: does not seem to work
    def closeThread(t: Thread, name: String) {
      while (t.isAlive()) {
        println(s"flashix: interrupting ${name}")
        t.interrupt
        println(s"flashix: joining ${name}")
        t.join(100)
      }
    }
    closeThread(erase, "erase")
    closeThread(wearleveling, "wear-leveling")
*/
  }
}

object Flashix {
  abstract class CachingStrategy
  case object NoCaching extends CachingStrategy
  case object WbufCaching extends CachingStrategy
  case object AfsCaching extends CachingStrategy
  
  abstract class ConcurrencyStrategy
  case object Sequential extends ConcurrencyStrategy
  case object WLConcurrent extends ConcurrencyStrategy
  case object GCConcurrent extends ConcurrencyStrategy
  case object ToplevelConcurrent extends ConcurrencyStrategy

  def filterArgs(args: Array[String]): (Array[String], CachingStrategy, ConcurrencyStrategy) = {
    var cachingStrategy: CachingStrategy = WbufCaching
    var concurrencyStrategy: ConcurrencyStrategy = Sequential
    
    val remainingsArgs = args.filter {
      case "-caching=none" =>
        cachingStrategy = NoCaching
        false
      case "-caching=wbuf" =>
        cachingStrategy = WbufCaching
        false
      case "-caching=afs" =>
        cachingStrategy = AfsCaching
        false
      case "-concurrency=none" =>
        concurrencyStrategy = Sequential
        false
      case "-concurrency=wl" =>
        concurrencyStrategy = WLConcurrent
        false
      case "-concurrency=gc" =>
        // TODO
        false
      case "-concurrency=external" =>
        // TODO
        false
      case _ =>
        true
    }
    (remainingsArgs, cachingStrategy, concurrencyStrategy)
  }
}
