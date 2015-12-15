// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._

object wlstatus extends helpers.scala.Random.Enumeration {
  type wlstatus = Value
  val used, free, erroneous, erasing = Value

  def uninit = free

  implicit object wlstatusRandomizer extends helpers.scala.Randomizer[wlstatus.wlstatus] {
    override def random(): wlstatus.wlstatus = wlstatus.random()
  }
}
