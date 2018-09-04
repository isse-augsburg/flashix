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

    // Format/Recover Flashix File System
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

    // Start concurrent erase/wear-leveling
    val wearleveling = new Thread {
      override def run {
        try {
          while (!this.isInterrupted()) {
            println("ubi: waiting for wear-leveling")
            val err: Ref[error] = Ref(error.ESUCCESS)
            val iswl: Ref[Boolean] = Ref(false)
            flashix.ubi.wear_leveling_worker(err, iswl)
            println("ubi: performed wear-leveling, err = " + err.get)
          }
        } catch {
          case _: InterruptedException =>
            println("ubi: wear-leveling thread interrupted")
        }
      }
    }
    val erase = new Thread {
      override def run {
        try {
          while (!this.isInterrupted()) {
            println("ubi: waiting for erase")
            flashix.ubi.erase_worker
            println("ubi: performed erasing")
          }
        } catch {
          case _: InterruptedException =>
            println("ubi: erase thread interrupted")
        }
      }
    }
    wearleveling.start
    erase.start

    // FUSE integration
    val filesystem = new fuse.FilesystemAdapter(flashix)
    val syncargs = Array("-s") ++ args
    FuseMount.mount(syncargs, filesystem, null)

    // Shutdown concurrent threads
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

    // Close MTD simulation
    mtd.close

    System.exit(0)
  }
}
