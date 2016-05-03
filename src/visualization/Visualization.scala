// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package visualization

import asm._
import types._
import types.error._
import helpers.scala._
import integration._
import java.io._
import _root_.fuse._
import scala.swing.event._
import scala.swing._
import visualization.Toolkit._
import visualization.models._
import scala.swing.TabbedPane.Page
import java.awt.GridLayout

trait Tab extends Component with Observer[Flashix] {
  def page: Page
}

object Visualization {
  def printHelp {
    println("usage:")
    println("  flashix [-odebug] [-obig_writes] <mountpoint>")
  }

  def main(args: Array[String]) {
    val err = new Ref(error.uninit)

    if (args.size <= 0) {
      printHelp
      System.exit(1)
    }

    // Implicit configuration options
    val deviceFile = new File("flash-device")
    val pebs = 1024
    val pages_per_peb = 64
    val page_size = 2048
    val spare_pebs = 5
    val doFormat = !deviceFile.exists()
    val doSync = false

    // Create MTD simulation
    val mtd = MTDSimulation(deviceFile, pebs, pages_per_peb, page_size)
    implicit val algebraic = new Algebraic(mtd)
    implicit val procedures = new Procedures()

    val flashix = new Flashix(mtd)

    object observable extends Observable[Flashix]

    def update() {
      observable update flashix
    }

    val refresh = check("Refresh", true, { if (_) update() })

    val filesystem = new fuse.FilesystemAdapter(flashix) {

      override def checked[A](operation: Ref[error] => A) = {
        flashix synchronized { super.checked(operation) }
      }
      override def _run(force: Boolean, sync: Boolean, operation: Ref[error] => Unit): Int = {
        val res = super._run(force, sync, operation)
        if (refresh.selected) update()
        res
      }
    }

    def format() {
      val rootmeta = fuse.DirMetadata()
      flashix synchronized {
        flashix.vfs.posix_format(pebs - spare_pebs, rootmeta, err)
      }
      if (err != ESUCCESS)
        println(s"vfs: format failed with error code ${err.get}")
      flashix.journal.SYNC = doSync // TODO: option
    }

    def recover() {
      flashix synchronized {
        flashix.vfs.posix_recover(err)
      }
      if (err != ESUCCESS)
        println(s"vfs: recovery failed with error code ${err.get}")
      flashix.journal.SYNC = doSync // TODO: option
    }

    def commit() {
      flashix synchronized {
        flashix.journal.aubifs_commit(err)
      }
      if (err != ESUCCESS)
        println(s"vfs: recovery failed with error code ${err.get}")
    }

    def dogc() {
      filesystem.doGC("user", err, -1)
    }

    def unmount() {
      Unmount.main(Array("-z", args.last))
      mtd.close
      System.exit(0)
    }

    val vis = List(Space, LPT, Index, WL)

    val fmt = button("Format", { format(); update() })
    val rec = button("Recover", { recover(); update() })
    val cm = button("Commit", { commit(); update() })
    val gc = button("GC", { dogc(); update() })
    val quit = button("Quit", { unmount() })

    val about = tab("About", label("Flashix File System"))

    val pages = about :: vis.map(_.page)
    val main = tabs(pages: _*)
    val side = vbox(refresh, cm, gc, rec, fmt, quit, Swing.VGlue)
    // side.peer.setLayout(new GridLayout(0,1))

    val window = frame("Flashix",
      hbox(main, side),
      { unmount() })

    window.size = new Dimension(600, 400)

    if (doFormat) {
      format()
    } else {
      recover()
    }

    if (err != ESUCCESS)
      System.exit(1)

    vis foreach (observable += _)

    update()

    val syncargs = Array("-s") ++ args
    FuseMount.mount(syncargs, filesystem, null)

    mtd.close
    System.exit(0)
  }
}
