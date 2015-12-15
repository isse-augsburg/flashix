package integration

import scala.sys.process.Process

object Unmount {
  def main(args: Array[String]): Unit = {
    Process("fusermount", "-u" :: args.toList).run
  }
}