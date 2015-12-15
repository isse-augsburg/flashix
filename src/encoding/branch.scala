// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package encoding

import helpers.scala._
import types._

package object branch {
  def flashsize(x: branch)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    val size = 1 + (x match {
      case _ : types.branch.mkbranch => 
        encoding.key.flashsize(x.key) + encoding.address.flashsize(x.adr)
      case _ : types.branch.mkentry => 
        encoding.key.flashsize(x.key) + encoding.address.flashsize(x.adr)
      case _ : types.branch.mkchecked => 
        encoding.key.flashsize(x.key)
    })
    size
  }

  def encode(x: branch, buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    val newIndex = x match {
      case _ : types.branch.mkbranch =>
        buf(index) = 0
      var curindex = index + 1
      curindex = encoding.key.encode(x.key, buf, curindex)
      curindex = encoding.address.encode(x.adr, buf, curindex)
      curindex
      case _ : types.branch.mkentry =>
        buf(index) = 1
      var curindex = index + 1
      curindex = encoding.key.encode(x.key, buf, curindex)
      curindex = encoding.address.encode(x.adr, buf, curindex)
      curindex
      case _ : types.branch.mkchecked =>
        buf(index) = 2
      var curindex = index + 1
      curindex = encoding.key.encode(x.key, buf, curindex)
      curindex
    }
    newIndex
  }

  def decode(buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): (branch, Int) = {
    import _implicit_algebraic._
    val (constructorIndex, newIndex) = helpers.scala.Encoding.decodeByte(buf, index)
    val (obj, curIndex) = constructorIndex match {
      case 0 =>
      val x0 = encoding.key.decode(buf, newIndex)
      val x1 = encoding.address.decode(buf, x0._2)
      (types.branch.mkbranch(x0._1, x1._1), x1._2)
      case 1 =>
      val x0 = encoding.key.decode(buf, newIndex)
      val x1 = encoding.address.decode(buf, x0._2)
      (types.branch.mkentry(x0._1, x1._1), x1._2)
      case 2 =>
      val x0 = encoding.key.decode(buf, newIndex)
      (types.branch.mkchecked(x0._1), x0._2)
      case _ =>
        throw helpers.scala.DecodeFailure()
    }
    (obj, curIndex)
}
}
