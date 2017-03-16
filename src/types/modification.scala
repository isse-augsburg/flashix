package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

sealed abstract class modification {
  def key : key = throw new InvalidSelector("key undefined")
  def adr : address = throw new InvalidSelector("adr undefined")
}

object modification {
  /**
   * case-classes and objects for constructors
   */
  final case class contains(override val key : key) extends modification {
  }
  final case class lookup(override val key : key) extends modification {
  }
  final case class store(override val key : key, override val adr : address) extends modification {
  }
  final case class remove(override val key : key) extends modification {
  }

  def uninit = contains(types.key.uninit)

  implicit object Randomizer extends helpers.scala.Randomizer[modification] {
    override def random(): modification = helpers.scala.Random.generator.nextInt(4) match {
      case 0 => contains(helpers.scala.Random[key])
      case 1 => lookup(helpers.scala.Random[key])
      case 2 => store(helpers.scala.Random[key], helpers.scala.Random[address])
      case 3 => remove(helpers.scala.Random[key])
    }
  }
}
