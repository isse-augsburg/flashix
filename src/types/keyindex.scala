package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

final case class keyindex(var key: Int, var idx: Int) extends DeepCopyable[keyindex] {
  override def deepCopy(): keyindex = keyindex(key, idx)

  def := (other: keyindex) {
    key = other.key
    idx = other.idx
  }
}

object keyindex {
  /**
   * Functions for constructors
   */
  def key_index(key: Int, idx: Int): keyindex = {
    keyindex(key, idx)
  }

  def uninit = key_index(0, 0)

  implicit object Randomizer extends helpers.scala.Randomizer[keyindex] {
    override def random(): keyindex = keyindex(helpers.scala.Random[Int], helpers.scala.Random[Int])
  }
}
