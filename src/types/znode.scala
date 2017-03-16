package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

final case class znode(var parent: znode, var zbranches: zbranch_array, var leaf: Boolean, var dirty: Boolean, var usedsize: Int) extends DeepCopyable[znode] {
  override def deepCopy(): znode = znode(parent, zbranches.deepCopy, leaf, dirty, usedsize)

  def := (other: znode) {
    parent = other.parent
    zbranches = other.zbranches
    leaf = other.leaf
    dirty = other.dirty
    usedsize = other.usedsize
  }
}

object znode {
  /**
   * Functions for constructors
   */
  def mkznode(parent: znode, zbranches: zbranch_array, leaf: Boolean, dirty: Boolean, usedsize: Int): znode = {
    znode(parent, zbranches, leaf, dirty, usedsize)
  }

  def uninit = mkznode(null, new zbranch_array(), helpers.scala.Boolean.uninit, helpers.scala.Boolean.uninit, 0)

  implicit object Randomizer extends helpers.scala.Randomizer[znode] {
    override def random(): znode = znode(helpers.scala.Random[znode], helpers.scala.Random[zbranch_array], helpers.scala.Random[Boolean], helpers.scala.Random[Boolean], helpers.scala.Random[Int])
  }
}
