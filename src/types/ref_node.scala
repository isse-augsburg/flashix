// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

final case class ref_node(leb: Int) {}

object ref_node {
  /**
   * Functions for constructors
   */
  def rnode(leb: Int): ref_node = {
    ref_node(leb)
  }

  def uninit = rnode(0)

  implicit object Randomizer extends helpers.scala.Randomizer[ref_node] {
    override def random(): ref_node = ref_node(helpers.scala.Random[Int])
  }
}
