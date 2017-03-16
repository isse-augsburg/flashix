package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

object wlstatus extends helpers.scala.Random.Enumeration {
  type wlstatus = Value
  val used, free, erroneous, erasing = Value

  def uninit = erasing

  implicit object wlstatusRandomizer extends helpers.scala.Randomizer[wlstatus.wlstatus] {
    override def random(): wlstatus.wlstatus = wlstatus.random()
  }
}
