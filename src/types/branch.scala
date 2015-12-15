package types

import helpers.scala._

sealed abstract class branch {
  def key : key = throw new InvalidSelector("key undefined")
  def updated_key(__x : key) : branch = throw new InvalidSelectorUpdate("updated_key undefined")
  def adr : address = throw new InvalidSelector("adr undefined")
  def updated_adr(__x : address) : branch = throw new InvalidSelectorUpdate("updated_adr undefined")
}

object branch {
  implicit object Randomizer extends helpers.scala.Randomizer[branch] {
    def random() : branch = mkbranch(helpers.scala.Random[key], helpers.scala.Random[address])
  }

  /**
   * case-classes and objects for constructors
   */
  final case class mkbranch(override val key : key, override val adr : address) extends branch {
    override def updated_key(__x : key) : mkbranch = copy(key = __x)
    override def updated_adr(__x : address) : mkbranch = copy(adr = __x)
  }
  final case class mkentry(override val key : key, override val adr : address) extends branch {
    override def updated_key(__x : key) : mkentry = copy(key = __x)
    override def updated_adr(__x : address) : mkentry = copy(adr = __x)
  }
  final case class mkchecked(override val key : key) extends branch {
    override def updated_key(__x : key) : mkchecked = copy(key = __x)
  }

  def uninit = mkbranch(key.uninit, address.uninit)
}
