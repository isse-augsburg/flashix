// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

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
    println("  flashix [-odebug] [-obig_writes] <mountpoint>")
  }

  def main(args: Array[String]) {

    if (args.size <= 0) {
      printHelp
      System.exit(1)
    }

    // Implicit configuration options
    val deviceFile = new File("flash-device")
    val pebs = 2 * 2048
    val pages_per_peb = 64
    val page_size = 2048
    val spare_pebs = 5
    val format = ! deviceFile.exists()

    // Create MTD simulation
    val mtd = MTDSimulation(deviceFile, pebs, pages_per_peb, page_size)
    implicit val algebraic = new Algebraic(mtd)
    implicit val procedures = new Procedures()

    // Check axioms
    procedures.flashix_check_axioms

    val ubiio = new ubi_io_asm(mtd)
    val ubi = new ubi_asm(new volumes(), new queue(), 0, new wlarray(), new nat_set(), 0, ubiio)
    val wbuf = new wbuf_asm(0, new wbuf_store(), ubi)
    val persistence_io = new persistence_io_asm(superblock.uninit, 0, wbuf)
    val persistence = new persistence_asm(binheap(new key_array(), 0), new nat_list(), new nat_list(), new lp_array(), persistence_io)
    val btree = new btree_asm(znode.uninit, address(0, 0, 0), persistence)
    val journal = new gjournal_asm(0, false, new nat_set(), 0, true, 0, btree) // TODO: sync option
    val aubifs = new aubifs_asm(journal)
    val vfs = new vfs_asm(new open_files(), 0, aubifs)

    val err = new Ref(error.uninit)
    if (format) {
      val rootmeta = fuse.DirMetadata()
      vfs.posix_format(pebs - spare_pebs, rootmeta, err)
      if (err != ESUCCESS)
        println(s"vfs: format failed with error code ${err.get}")
    } else {
      vfs.posix_recover(err)
      if (err != ESUCCESS)
        println(s"vfs: recovery failed with error code ${err.get}")
    }

    if (err != ESUCCESS)
      System.exit(1)

    val filesystem = new fuse.FilesystemAdapter(vfs, journal, persistence, persistence_io)
    val syncargs = Array("-s") ++ args
    FuseMount.mount(syncargs, filesystem, null)

    mtd.close
    System.exit(0)
  }
}
