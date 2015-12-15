// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package encoding

import helpers.scala._
import types._

package object superblock {
  def flashsize(x: superblock)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    LEB_SIZE
}

  def encode(x: superblock, buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    var curindex = index
    curindex = encoding.address.encode(x.indexaddr, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.maxino, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.orph, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.lpt, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.log, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.main, buf, curindex)
    assert(curindex - (index) <= LEB_SIZE)
    return index + LEB_SIZE
  }

  def decode(buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): (superblock, Int) = {
    import _implicit_algebraic._
    val x0 = encoding.address.decode(buf, index)
    val x1 = helpers.scala.Encoding.decodeNat(buf, x0._2)
    val x2 = helpers.scala.Encoding.decodeNat(buf, x1._2)
    val x3 = helpers.scala.Encoding.decodeNat(buf, x2._2)
    val x4 = helpers.scala.Encoding.decodeNat(buf, x3._2)
    val x5 = helpers.scala.Encoding.decodeNat(buf, x4._2)
    assert(x5._2 - index <= LEB_SIZE)
    (types.superblock.mksb(x0._1, x1._1, x2._1, x3._1, x4._1, x5._1), index + LEB_SIZE)
  }
}
