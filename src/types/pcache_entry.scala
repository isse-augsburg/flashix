// Flashix: a verified file system for flash memory
// (c) 2015-2019 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._

final case class pcache_entry(var dirty: Boolean, var page: buffer) extends DeepCopyable[pcache_entry] {
  override def deepCopy(): pcache_entry = pcache_entry(dirty, page.deepCopy)

  def := (other: pcache_entry) {
    dirty = other.dirty
    page = other.page
  }
}

object pcache_entry {
  /**
   * Functions for constructors
   */
  def P(dirty: Boolean, page: buffer): pcache_entry = {
    pcache_entry(dirty, page)
  }

  def uninit = P(helpers.scala.Boolean.uninit, new buffer())

  implicit object Randomizer extends helpers.scala.Randomizer[pcache_entry] {
    override def random(): pcache_entry = pcache_entry(helpers.scala.Random[Boolean], helpers.scala.Random[buffer])
  }
}
