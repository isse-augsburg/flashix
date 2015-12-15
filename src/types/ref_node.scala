package types

import helpers.scala._

final case class ref_node(lnum: Int) {}

object ref_node {
  /**
   * Functions for constructors
   */
  def rnode(lnum: Int) : ref_node = {
    ref_node(lnum)
  }

  def uninit = rnode(0)

  implicit object Randomizer extends helpers.scala.Randomizer[ref_node] {
    def random() : ref_node = ref_node(helpers.scala.Random[Int])
  }
}
