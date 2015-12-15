package encoding

import helpers.scala._
import types._

package object ref_node {
  def flashsize(x: ref_node)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    EB_PAGE_SIZE
}

  def encode(x: ref_node, buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    var curindex = index
    curindex = helpers.scala.Encoding.encode(x.lnum, buf, curindex)
    assert(curindex - (index) <= EB_PAGE_SIZE)
    return index + EB_PAGE_SIZE
  }

  def decode(buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): (ref_node, Int) = {
    import _implicit_algebraic._
    val x0 = helpers.scala.Encoding.decodeNat(buf, index)
    assert(x0._2 - index <= EB_PAGE_SIZE)
    (types.ref_node.rnode(x0._1), index + EB_PAGE_SIZE)
  }
}
