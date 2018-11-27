// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package visualization

import types._
import types.error._
import helpers.scala._
import integration._
import java.io._
import _root_.fuse._
import scala.swing._
import visualization.Toolkit._
import visualization.models._
import scala.swing.TabbedPane.Page
import test.TestGC

trait Tab extends Component with Observer[Flashix] {
  def page: Page
}

object Visualization {
  def printHelp(): Unit = {
    println("usage:")
    println("  flashix [-caching=none/wbuf/afs] [-odebug] [-obig_writes] <mountpoint>")
  }

  def main(initialArgs: Array[String]) {
    val err = new Ref(error.uninit)

    if (initialArgs.length <= 0) {
      printHelp()
      System.exit(1)
    }

    val (args, cachingStrategy) = Flashix.filterArgs(initialArgs)

    // Implicit configuration options
    val deviceFile = new File("flash-device")
    val pebs = 1024
    val pages_per_peb = 256
    val page_size = 2048
    val spare_pebs = 5
    val doFormat = !deviceFile.exists()
    val doSync = false
    val testGC = new TestGC(args.last, page_size, pages_per_peb / 2)
    var testGCThread: Thread = null

    // Create MTD simulation
    val mtd = MTDSimulation(deviceFile, pebs, pages_per_peb, page_size)
    implicit val algebraic: Algebraic = new Algebraic(mtd)
    implicit val procedures: Procedures = new Procedures()

    object observable extends Observable[Flashix]
    def update0(flashix: Flashix): Unit = {
      flashix synchronized {
        observable update flashix
      }
    }
    val flashix = new Flashix(cachingStrategy, update0, mtd)
    def update() { update0(flashix) }

    val refresh = check("Refresh", true, { if (_) update() })

    val filesystem = new fuse.FilesystemAdapter(flashix) {
      override def checked[A](operation: Ref[error] => A): A = {
        flashix synchronized { super.checked(operation) }
      }
      override def _run(force: Boolean, operation: Ref[error] => Unit): Int = {
        val res = super._run(force, operation)
        if (refresh.selected) update()
        res
      }
    }

    def dosync() {
      flashix synchronized {
        flashix.posix.sync(err)
      }
    }

    def format() {
      val rootmeta = fuse.DirMetadata()
      flashix synchronized {
        flashix.joinConcurrentOps
        flashix.vfs.format(pebs - spare_pebs, doSync, (pages_per_peb - 2) * page_size, rootmeta, err)
        flashix.startConcurrentOps
      }
      if (err.get != ESUCCESS)
        println(s"vfs: format failed with error code ${err.get}")
    }

    def recover() {
      flashix synchronized {
        flashix.joinConcurrentOps
        flashix.vfs.recover(doSync, err)
        flashix.startConcurrentOps
      }
      if (err.get != ESUCCESS)
        println(s"vfs: recovery failed with error code ${err.get}")
    }

    def commit() {
      flashix synchronized {
        flashix.journal.commit(err)
      }
      if (err.get != ESUCCESS)
        println(s"vfs: commit failed with error code ${err.get}")
    }

    def dogc() {
      filesystem.doGC("user", err, -1)
    }

    def toggleTestGC(): Unit = {
      if (testGCThread != null && testGCThread.isAlive) {
        testGC.stop()
        testGCThread.join()
        testGCThread = null
      } else {
        testGCThread = new Thread(testGC)
        testGCThread.start()
      }
    }

    def unmount() {
      if (testGCThread != null && testGCThread.isAlive) {
        testGC.stop()
        testGCThread.join()
      }
      Unmount.main(Array("-z", args.last))
      mtd.close
      System.exit(0)
    }

    val vis = List(Space, LPT, Index, Log, WL)

    val fmt = button("Format", { format(); update() })
    val rec = button("Recover", { recover(); update() })
    val cm = button("Commit", { commit(); update() })
    val gc = button("GC", { dogc(); update() })
    val btnTestGC = button("TestGC Start/Stop", { toggleTestGC(); update() })
    val sync = button("Sync", { dosync(); update() })
    val quit = button("Quit", { unmount() })

    val about = tab("About", label("Flashix File System"))

    val pages = about :: vis.map(_.page)
    val main = tabs(pages: _*)
    val side = vbox(refresh, sync, cm, gc, btnTestGC, rec, fmt, quit, Swing.VGlue)
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

    if (err.get != ESUCCESS)
      System.exit(1)

    vis foreach { observable += _ }

    update()

    val syncargs = Array("-f", "-s") ++ args
    FuseMount.mount(syncargs, filesystem, null)

    // Shutdown concurrent threads
    flashix.joinConcurrentOps

    mtd.close
    System.exit(0)
  }
}
