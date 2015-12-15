package types

import helpers.scala._

final case class group_node(nd: node, sqnum: Int, start: Boolean, end: Boolean) extends DeepCopyable[group_node] {
  override def deepCopy(): group_node = group_node(nd.deepCopy, sqnum, start, end)
}

object group_node {
  /**
   * Functions for constructors
   */
  def mkgnode(nd: node, sqnum: Int, start: Boolean, end: Boolean) : group_node = {
    group_node(nd, sqnum, start, end)
  }

  def uninit = mkgnode(node.uninit, 0, helpers.scala.Boolean.uninit, helpers.scala.Boolean.uninit)

  implicit object Randomizer extends helpers.scala.Randomizer[group_node] {
    def random() : group_node = group_node(helpers.scala.Random[node], helpers.scala.Random[Int], helpers.scala.Random[Boolean], helpers.scala.Random[Boolean])
  }
}
