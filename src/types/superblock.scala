// Flashix: a verified file system for flash memory
// (c) 2015-2019 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._

final case class superblock(var indexaddr: address, var maxino: Int, var log: Int, var orph: Int, var orphsize: Int, var lpt: Int, var main: Int) extends DeepCopyable[superblock] {
  override def deepCopy(): superblock = superblock(indexaddr, maxino, log, orph, orphsize, lpt, main)

  def := (other: superblock) {
    indexaddr = other.indexaddr
    maxino = other.maxino
    log = other.log
    orph = other.orph
    orphsize = other.orphsize
    lpt = other.lpt
    main = other.main
  }
}

object superblock {
  /**
   * Functions for constructors
   */
  def mksb(indexaddr: address, maxino: Int, log: Int, orph: Int, orphsize: Int, lpt: Int, main: Int): superblock = {
    superblock(indexaddr, maxino, log, orph, orphsize, lpt, main)
  }

  def uninit = mksb(types.address.uninit, 0, 0, 0, 0, 0, 0)

  implicit object Randomizer extends helpers.scala.Randomizer[superblock] {
    override def random(): superblock = superblock(helpers.scala.Random[address], helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[Int], helpers.scala.Random[Int])
  }
}
