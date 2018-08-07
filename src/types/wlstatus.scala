// Flashix: a verified file system for flash memory
// (c) 2015-2018 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._

object wlstatus extends helpers.scala.Random.Enumeration {
  type wlstatus = Value
  val used, free, erroneous, erasing = Value

  def uninit = erasing

  implicit object wlstatusRandomizer extends helpers.scala.Randomizer[wlstatus.wlstatus] {
    override def random(): wlstatus.wlstatus = wlstatus.random()
  }
}
