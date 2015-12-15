package types

import helpers.scala._

final case class metadata(mode: Int, uid: Int, gid: Int, atime: Int, atimesec: Int, mtime: Int, mtimesec: Int, ctime: Int, ctimesec: Int) {}

object metadata {
  /**
   * Functions for constructors
   */
  def mkmetadata(mode: Int, uid: Int, gid: Int, atime: Int, atimesec: Int, mtime: Int, mtimesec: Int, ctime: Int, ctimesec: Int) : metadata = {
    metadata(mode, uid, gid, atime, atimesec, mtime, mtimesec, ctime, ctimesec)
  }

  def uninit = mkmetadata(0, 0, 0, 0, 0, 0, 0, 0, 0)

  implicit object Randomizer extends helpers.scala.Randomizer[metadata] {
    def random() : metadata = metadata(helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[Int])
  }
}
