// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package encoding

import helpers.scala._
import types._

package object vidheader {
  def flashsize(x: vidheader)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    EB_PAGE_SIZE
}

  def encode(x: vidheader, buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    var curindex = index
    curindex = helpers.scala.Encoding.encode(x.vol, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.leb, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.sqn, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.size, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.checksum, buf, curindex)
    assert(curindex - (index) <= EB_PAGE_SIZE)
    return index + EB_PAGE_SIZE
  }

  def decode(buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): (vidheader, Int) = {
    import _implicit_algebraic._
    val x0 = helpers.scala.Encoding.decodeByte(buf, index)
    val x1 = helpers.scala.Encoding.decodeNat(buf, x0._2)
    val x2 = helpers.scala.Encoding.decodeNat(buf, x1._2)
    val x3 = helpers.scala.Encoding.decodeNat(buf, x2._2)
    val x4 = helpers.scala.Encoding.decodeNat(buf, x3._2)
    assert(x4._2 - index <= EB_PAGE_SIZE)
    (types.vidheader.vidhdr(x0._1, x1._1, x2._1, x3._1, x4._1), index + EB_PAGE_SIZE)
  }
}
