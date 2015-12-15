// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package integration

import scala.sys.process.Process

object Unmount {
  def main(args: Array[String]): Unit = {
    Process("fusermount", "-u" :: args.toList).run
  }
}