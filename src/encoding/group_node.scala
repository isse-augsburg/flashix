package encoding

import helpers.scala._
import types._

package object group_node {
  def flashsize(x: group_node)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    helpers.scala.Encoding.alignUp(encoding.node.flashsize(x.nd) + helpers.scala.Encoding.flashsize(x.sqnum) + helpers.scala.Encoding.flashsize(x.start) + helpers.scala.Encoding.flashsize(x.end), 2 * NODE_HEADER_SIZE)
}

  def encode(x: group_node, buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    var curindex = index
    curindex = encoding.node.encode(x.nd, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.sqnum, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.start, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.end, buf, curindex)
    (index) + helpers.scala.Encoding.alignUp(curindex - (index), 2 * NODE_HEADER_SIZE)
  }

  def decode(buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): (group_node, Int) = {
    import _implicit_algebraic._
    val x0 = encoding.node.decode(buf, index)
    val x1 = helpers.scala.Encoding.decodeNat(buf, x0._2)
    val x2 = helpers.scala.Encoding.decodeBoolean(buf, x1._2)
    val x3 = helpers.scala.Encoding.decodeBoolean(buf, x2._2)
    (types.group_node.mkgnode(x0._1, x1._1, x2._1, x3._1), index + helpers.scala.Encoding.alignUp(x3._2 - index, 2 * NODE_HEADER_SIZE))
  }
}
