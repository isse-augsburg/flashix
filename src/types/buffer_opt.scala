// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

sealed abstract class buffer_opt extends DeepCopyable[buffer_opt] {
  def buf : buffer = throw new InvalidSelector("buf undefined")
}

object buffer_opt {
  /**
   * case-classes and objects for constructors
   */
  final case class some(override val buf : buffer) extends buffer_opt {
    override def deepCopy(): buffer_opt = some(buf.deepCopy)
  }
  final object none extends buffer_opt {
    override def deepCopy(): buffer_opt = this
  }

  def uninit = none

  implicit object Randomizer extends helpers.scala.Randomizer[buffer_opt] {
    override def random(): buffer_opt = helpers.scala.Random.generator.nextInt(2) match {
      case 0 => some(helpers.scala.Random[buffer])
      case 1 => none
    }
  }
}
