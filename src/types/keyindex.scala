// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._

final case class keyindex(key: Int, idx: Int) {}

object keyindex {
  /**
   * Functions for constructors
   */
  def key_index(key: Int, idx: Int) : keyindex = {
    keyindex(key, idx)
  }

  def uninit = key_index(0, 0)

  implicit object Randomizer extends helpers.scala.Randomizer[keyindex] {
    def random() : keyindex = keyindex(helpers.scala.Random[Int], helpers.scala.Random[Int])
  }
}
