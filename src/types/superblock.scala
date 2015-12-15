package types

import helpers.scala._

final case class superblock(indexaddr: address, maxino: Int, orph: Int, lpt: Int, log: Int, main: Int) {}

object superblock {
  /**
   * Functions for constructors
   */
  def mksb(indexaddr: address, maxino: Int, orph: Int, lpt: Int, log: Int, main: Int) : superblock = {
    superblock(indexaddr, maxino, orph, lpt, log, main)
  }

  def uninit = mksb(address.uninit, 0, 0, 0, 0, 0)

  implicit object Randomizer extends helpers.scala.Randomizer[superblock] {
    def random() : superblock = superblock(helpers.scala.Random[address], helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[Int])
  }
}
