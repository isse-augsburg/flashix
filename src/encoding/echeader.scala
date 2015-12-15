// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package encoding

import helpers.scala._
import types._

package object echeader {
  def flashsize(x: echeader)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    EB_PAGE_SIZE
}

  def encode(x: echeader, buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    var curindex = index
    curindex = helpers.scala.Encoding.encode(x.ec, buf, curindex)
    assert(curindex - (index) <= EB_PAGE_SIZE)
    return index + EB_PAGE_SIZE
  }

  def decode(buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): (echeader, Int) = {
    import _implicit_algebraic._
    val x0 = helpers.scala.Encoding.decodeNat(buf, index)
    assert(x0._2 - index <= EB_PAGE_SIZE)
    (types.echeader.echdr(x0._1), index + EB_PAGE_SIZE)
  }
}
