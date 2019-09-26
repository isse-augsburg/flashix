// Flashix: a verified file system for flash memory
// (c) 2015-2019 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._

final case class node_header(size: Int, ispadding: Boolean) {}

object node_header {
  /**
   * Functions for constructors
   */
  def nodeheader(size: Int, ispadding: Boolean): node_header = {
    node_header(size, ispadding)
  }

  def uninit = nodeheader(0, helpers.scala.Boolean.uninit)

  implicit object Randomizer extends helpers.scala.Randomizer[node_header] {
    override def random(): node_header = node_header(helpers.scala.Random[Int], helpers.scala.Random[Boolean])
  }
}
