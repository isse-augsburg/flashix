// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package encoding

import helpers.scala._
import types._

package object node_header {
  def flashsize(x: node_header)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    NODE_HEADER_SIZE
}

  def encode(x: node_header, buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    var curindex = index
    curindex = helpers.scala.Encoding.encode(x.size, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.ispadding, buf, curindex)
    assert(curindex - (index) <= NODE_HEADER_SIZE)
    return index + NODE_HEADER_SIZE
  }

  def decode(buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): (node_header, Int) = {
    import _implicit_algebraic._
    val x0 = helpers.scala.Encoding.decodeNat(buf, index)
    val x1 = helpers.scala.Encoding.decodeBoolean(buf, x0._2)
    assert(x1._2 - index <= NODE_HEADER_SIZE)
    (types.node_header.nodeheader(x0._1, x1._1), index + NODE_HEADER_SIZE)
  }
}
