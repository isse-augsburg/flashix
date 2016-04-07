// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

sealed abstract class modification extends DeepCopyable[modification] {
  def key : key = throw new InvalidSelector("key undefined")
  def adr : address = throw new InvalidSelector("adr undefined")
  def nd : node = throw new InvalidSelector("nd undefined")
}

object modification {
  /**
   * case-classes and objects for constructors
   */
  final case class contains(override val key : key) extends modification {
    override def deepCopy(): modification = contains(key)
  }
  final case class lookup(override val key : key) extends modification {
    override def deepCopy(): modification = lookup(key)
  }
  final case class check(override val key : key) extends modification {
    override def deepCopy(): modification = check(key)
  }
  final case class store(override val key : key, override val adr : address, override val nd : node) extends modification {
    override def deepCopy(): modification = store(key, adr, nd.deepCopy)
  }
  final case class remove(override val key : key) extends modification {
    override def deepCopy(): modification = remove(key)
  }

  def uninit = contains(types.key.uninit)

  implicit object Randomizer extends helpers.scala.Randomizer[modification] {
    override def random(): modification = helpers.scala.Random.generator.nextInt(5) match {
      case 0 => contains(helpers.scala.Random[key])
      case 1 => lookup(helpers.scala.Random[key])
      case 2 => check(helpers.scala.Random[key])
      case 3 => store(helpers.scala.Random[key], helpers.scala.Random[address], helpers.scala.Random[node])
      case 4 => remove(helpers.scala.Random[key])
    }
  }
}
