// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

final case class index_node(var branches: branch_array, var leaf: Boolean, var usedsize: Int) extends DeepCopyable[index_node] {
  override def deepCopy(): index_node = index_node(branches.deepCopy, leaf, usedsize)

  def := (other: index_node) {
    branches = other.branches
    leaf = other.leaf
    usedsize = other.usedsize
  }
}

object index_node {
  /**
   * Functions for constructors
   */
  def indexnode(branches: branch_array, leaf: Boolean, usedsize: Int): index_node = {
    index_node(branches, leaf, usedsize)
  }

  def uninit = indexnode(new branch_array(), helpers.scala.Boolean.uninit, 0)

  implicit object Randomizer extends helpers.scala.Randomizer[index_node] {
    override def random(): index_node = index_node(helpers.scala.Random[branch_array], helpers.scala.Random[Boolean], helpers.scala.Random[Int])
  }
}
