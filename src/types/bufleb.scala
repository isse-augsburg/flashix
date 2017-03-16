package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

sealed abstract class bufleb {
  def leb : Int = throw new InvalidSelector("leb undefined")
}

object bufleb {
  /**
   * case-classes and objects for constructors
   */
  final case class buffered(override val leb : Int) extends bufleb {
  }
  final object nobuffer extends bufleb

  def uninit = nobuffer

  implicit object Randomizer extends helpers.scala.Randomizer[bufleb] {
    override def random(): bufleb = helpers.scala.Random.generator.nextInt(2) match {
      case 0 => nobuffer
      case 1 => buffered(helpers.scala.Random[Int])
    }
  }
}
