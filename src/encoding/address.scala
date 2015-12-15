// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package encoding

import helpers.scala._
import types._

package object address {
  def flashsize(x: address)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    helpers.scala.Encoding.flashsize(x.lnum) + helpers.scala.Encoding.flashsize(x.pos) + helpers.scala.Encoding.flashsize(x.size)
}

  def encode(x: address, buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    var curindex = index
    curindex = helpers.scala.Encoding.encode(x.lnum, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.pos, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.size, buf, curindex)
    curindex
  }

  def decode(buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): (address, Int) = {
    import _implicit_algebraic._
    val x0 = helpers.scala.Encoding.decodeNat(buf, index)
    val x1 = helpers.scala.Encoding.decodeNat(buf, x0._2)
    val x2 = helpers.scala.Encoding.decodeNat(buf, x1._2)
    (types.address.at(x0._1, x1._1, x2._1), x2._2)
  }
}
