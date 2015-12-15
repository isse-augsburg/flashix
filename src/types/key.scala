package types

import helpers.scala._

sealed abstract class key {
  def ino : Int = throw new InvalidSelector("ino undefined")
  def updated_ino(__x : Int) : key = throw new InvalidSelectorUpdate("updated_ino undefined")
  def part : Int = throw new InvalidSelector("part undefined")
  def updated_part(__x : Int) : key = throw new InvalidSelectorUpdate("updated_part undefined")
  def name : String = throw new InvalidSelector("name undefined")
  def updated_name(__x : String) : key = throw new InvalidSelectorUpdate("updated_name undefined")
}

object key {
  implicit object Randomizer extends helpers.scala.Randomizer[key] {
    def random() : key = inodekey(helpers.scala.Random[Int])
  }

  /**
   * case-classes and objects for constructors
   */
  final case class inodekey(override val ino : Int) extends key {
    override def updated_ino(__x : Int) : inodekey = copy(ino = __x)
  }
  final case class datakey(override val ino : Int, override val part : Int) extends key {
    override def updated_ino(__x : Int) : datakey = copy(ino = __x)
    override def updated_part(__x : Int) : datakey = copy(part = __x)
  }
  final case class dentrykey(override val ino : Int, override val name : String) extends key {
    override def updated_ino(__x : Int) : dentrykey = copy(ino = __x)
    override def updated_name(__x : String) : dentrykey = copy(name = __x)
  }

  def uninit = inodekey(0)
}
