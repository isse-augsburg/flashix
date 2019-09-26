// Flashix: a verified file system for flash memory
// (c) 2015-2019 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._

final case class mscache_entry(meta: metadata, size: Int) {}

object mscache_entry {
  /**
   * Functions for constructors
   */
  def MS(meta: metadata, size: Int): mscache_entry = {
    mscache_entry(meta, size)
  }

  def uninit = MS(types.metadata.uninit, 0)

  implicit object Randomizer extends helpers.scala.Randomizer[mscache_entry] {
    override def random(): mscache_entry = mscache_entry(helpers.scala.Random[metadata], helpers.scala.Random[Int])
  }
}
