// Flashix: a verified file system for flash memory
// (c) 2015-2019 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._
import types.wlstatus.wlstatus

final case class wlentry(var ec: Int, var status: wlstatus) extends DeepCopyable[wlentry] {
  override def deepCopy(): wlentry = wlentry(ec, status)

  def := (other: wlentry) {
    ec = other.ec
    status = other.status
  }
}

object wlentry {
  /**
   * Functions for constructors
   */
  def wl_entry(ec: Int, status: wlstatus): wlentry = {
    wlentry(ec, status)
  }

  def uninit = wl_entry(0, types.wlstatus.uninit)

  implicit object Randomizer extends helpers.scala.Randomizer[wlentry] {
    override def random(): wlentry = wlentry(helpers.scala.Random[Int], helpers.scala.Random[wlstatus])
  }
}
