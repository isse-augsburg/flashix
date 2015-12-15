// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._

object lpropflags extends helpers.scala.Random.Enumeration {
  type lpropflags = Value
  val LP_FREE, LP_GROUP_NODES, LP_INDEX_NODES = Value

  def uninit = LP_INDEX_NODES

  implicit object lpropflagsRandomizer extends helpers.scala.Randomizer[lpropflags.lpropflags] {
    override def random(): lpropflags.lpropflags = lpropflags.random()
  }
}
