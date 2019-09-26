// Flashix: a verified file system for flash memory
// (c) 2015-2019 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._

sealed abstract class tcache_entry {
  def minuptrunc : Int = throw new InvalidSelector("minuptrunc undefined")
  def updated_minuptrunc(__x : Int) : tcache_entry = throw new InvalidSelectorUpdate("updated_minuptrunc undefined")
  def lasttrunc : Int = throw new InvalidSelector("lasttrunc undefined")
  def updated_lasttrunc(__x : Int) : tcache_entry = throw new InvalidSelectorUpdate("updated_lasttrunc undefined")
}

object tcache_entry {
  /**
   * case-classes and objects for constructors
   */
  final case class T(override val minuptrunc : Int, override val lasttrunc : Int) extends tcache_entry {
    override def updated_minuptrunc(__x : Int) : T = copy(minuptrunc = __x)
    override def updated_lasttrunc(__x : Int) : T = copy(lasttrunc = __x)
  }
  final case class T0(override val lasttrunc : Int) extends tcache_entry {
    override def updated_lasttrunc(__x : Int) : T0 = copy(lasttrunc = __x)
  }

  def uninit = T(0, 0)

  implicit object Randomizer extends helpers.scala.Randomizer[tcache_entry] {
    override def random(): tcache_entry = helpers.scala.Random.generator.nextInt(2) match {
      case 0 => T(helpers.scala.Random[Int], helpers.scala.Random[Int])
      case 1 => T0(helpers.scala.Random[Int])
    }
  }
}
