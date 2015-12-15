package types

import helpers.scala._
import types.wlstatus.wlstatus

final case class wlentry(var ec: Int, var status: wlstatus) extends DeepCopyable[wlentry] {
  override def deepCopy(): wlentry = wlentry(ec, status)
}

object wlentry {
  /**
   * Functions for constructors
   */
  def wl_entry(ec: Int, status: wlstatus) : wlentry = {
    wlentry(ec, status)
  }

  def uninit = wl_entry(0, wlstatus.uninit)

  implicit object Randomizer extends helpers.scala.Randomizer[wlentry] {
    def random() : wlentry = wlentry(helpers.scala.Random[Int], helpers.scala.Random[wlstatus])
  }
}
