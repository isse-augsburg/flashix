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
import visualization.Visualization

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
    val format = !deviceFile.exists()

    // Create MTD simulation
    val mtd = MTDSimulation(deviceFile, pebs, pages_per_peb, page_size)
    implicit val algebraic = new Algebraic(mtd)
    implicit val procedures = new Procedures()

    val flashix = new Flashix(mtd)
    val dosync = false
    val err = new Ref(error.uninit)
    if (format) {
      val rootmeta = fuse.DirMetadata()
      flashix.vfs.format(pebs - spare_pebs, dosync, (pages_per_peb - 2) * page_size, rootmeta, err)
      if (err != ESUCCESS)
        println(s"vfs: format failed with error code ${err.get}")
    } else {
      flashix.vfs.recover(dosync, err)
      if (err != ESUCCESS)
        println(s"vfs: recovery failed with error code ${err.get}")
    }

    if (err != ESUCCESS)
      System.exit(1)

    val filesystem = new fuse.FilesystemAdapter(flashix)

    val syncargs = Array("-s") ++ args
    FuseMount.mount(syncargs, filesystem, null)

    mtd.close
    System.exit(0)
  }
}
