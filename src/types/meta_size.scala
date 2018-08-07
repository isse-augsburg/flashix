// Flashix: a verified file system for flash memory
// (c) 2015-2018 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._

final case class meta_size(meta: metadata, size: Int) {}

object meta_size {
  /**
   * Functions for constructors
   */
  def metasize(meta: metadata, size: Int): meta_size = {
    meta_size(meta, size)
  }

  def uninit = metasize(types.metadata.uninit, 0)

  implicit object Randomizer extends helpers.scala.Randomizer[meta_size] {
    override def random(): meta_size = meta_size(helpers.scala.Random[metadata], helpers.scala.Random[Int])
  }
}
