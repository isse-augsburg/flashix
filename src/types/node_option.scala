// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._

sealed abstract class node_option extends DeepCopyable[node_option] {
  def get : node = throw new InvalidSelector("get undefined")
}

object node_option {
  implicit object Randomizer extends helpers.scala.Randomizer[node_option] {
    def random() : node_option = none
  }

  /**
   * case-classes and objects for constructors
   */
  final case class some(override val get : node) extends node_option {
    override def deepCopy(): node_option = some(get.deepCopy)
  }
  final object none extends node_option {
    override def deepCopy(): node_option = this
  }

  def uninit = none
}
