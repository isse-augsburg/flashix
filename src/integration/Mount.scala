package integration

import asm._
import types._
import types.error._
import helpers.scala._
import java.io._
import _root_.fuse._

object Mount {
  def native = true

  def printHelp {
    println("usage:")
    println("  flashix <mountpoint>")
  }

  def main(args: Array[String]) {

    // Check arguments
    if (args.size != 1) {
      println("error: invalid number of arguments")
      printHelp
      System.exit(1)
    }
    val fuseMountpoint = new File(args(0))
    if (! fuseMountpoint.exists()) {
      println(s"error: mountpoint ${args(0)} does not exist")
      printHelp
      System.exit(1)
    }
    if (! fuseMountpoint.isDirectory()) {
      println(s"error: mountpoint ${args(1)} is not a directory")
      printHelp
      System.exit(1)
    }

    // Implicit configuration options
    val deviceFile = new File("flash-device")
    val pebs = 2048
    val pages_per_peb = 64
    val page_size = 2048
    val spare_pebs = 5
    val format = ! deviceFile.exists()

    // Create MTD simulation
    val mtd = MTDSimulation(deviceFile, pebs, pages_per_peb, page_size)
    implicit val algebraic = new Algebraic(mtd)
    val ubiio = UBIIO(mtd) 
    val ubi = UBI(new nat_set(), new wlarray(), 0, new queue(), new volumes(), 0, ubiio)
    val wbuf = WBUF(new wbuf_store(), 0, ubi)
    val persistence = Persistence(new nat_list(), true, superblock(address(0, 0, 0), 0, 0, 0, 0, 0), 0, binheap(new key_array(), 0), new lp_array(), new nat_list(), wbuf)
    val journal = UBIFSJournal(false, address(0, 0, 0), new key_set(), 0, 0, 0, null, persistence)
    val aubifs = AUBIFS(journal)
    val vfs = VFS(new open_files(), aubifs)

    val err = new Ref(error.uninit)
    if (format) {
      val rootmeta = fuse.DirMetadata()
      vfs.posix_format(pebs - spare_pebs, rootmeta, err)
      if (err != ESUCCESS)
        println(s"vfs: format failed with error code ${err.get}")
    } else {
      ubi.ubi_recover(err)
      if (err != ESUCCESS)
        println(s"ubi: recovery failed with error code ${err.get}")
      else {
        wbuf.wbuf_recover(err)
        if (err != ESUCCESS)
          println(s"wbuf: recovery failed with error code ${err.get}")
        else {
          persistence.ubifs_persistence_recover(err)
          if (err != ESUCCESS)
            println(s"persistence: recovery failed with error code ${err.get}")
          else {
            aubifs.aubifs_replay(err)
            if (err != ESUCCESS)
              println(s"aubifs: recovery failed with error code ${err.get}")
          }
        }
      }
    }

    if (err != ESUCCESS)
      System.exit(1)

    val filesystem = new fuse.FilesystemAdapter(vfs, journal, persistence)
    FuseMount.mount(Array("-s", "-odebug", "-obig_writes", args(0)), filesystem, null)

    mtd.close
    System.exit(0)
  }
}
