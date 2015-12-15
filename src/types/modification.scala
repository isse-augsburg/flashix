package types

import helpers.scala._

sealed abstract class modification extends DeepCopyable[modification] {
  def key : key = throw new InvalidSelector("key undefined")
  def adr : address = throw new InvalidSelector("adr undefined")
  def nd : node = throw new InvalidSelector("nd undefined")
}

object modification {
  implicit object Randomizer extends helpers.scala.Randomizer[modification] {
    def random() : modification = contains(helpers.scala.Random[key])
  }

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

  def uninit = contains(key.uninit)
}
