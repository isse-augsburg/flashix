package encoding

import helpers.scala._
import types._

package object index_node {
  def flashsize(x: index_node)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    helpers.scala.Encoding.alignUp((helpers.scala.Encoding.flashsizeArrayWrapper[branch](_: branch_array, encoding.branch.flashsize))(x.branches) + helpers.scala.Encoding.flashsize(x.leaf) + helpers.scala.Encoding.flashsize(x.usedsize), 2 * NODE_HEADER_SIZE)
}

  def encode(x: index_node, buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    var curindex = index
    curindex = (helpers.scala.Encoding.encodeArrayWrapper[branch](_: branch_array, _: Array[Byte], _: Int, encoding.branch.encode))(x.branches, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.leaf, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.usedsize, buf, curindex)
    (index) + helpers.scala.Encoding.alignUp(curindex - (index), 2 * NODE_HEADER_SIZE)
  }

  def decode(buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): (index_node, Int) = {
    import _implicit_algebraic._
    val x0 = (helpers.scala.Encoding.decodeArrayWrapper[branch](_: Array[Byte], _: Int, encoding.branch.decode))(buf, index)
    val x1 = helpers.scala.Encoding.decodeBoolean(buf, x0._2)
    val x2 = helpers.scala.Encoding.decodeNat(buf, x1._2)
    (types.index_node.indexnode(x0._1, x1._1, x2._1), index + helpers.scala.Encoding.alignUp(x2._2 - index, 2 * NODE_HEADER_SIZE))
  }
}
