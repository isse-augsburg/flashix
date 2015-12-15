package encoding

import helpers.scala._
import types._

package object key {
  def flashsize(x: key)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    val size = 1 + (x match {
      case _ : types.key.inodekey => 
        helpers.scala.Encoding.flashsize(x.ino)
      case _ : types.key.datakey => 
        helpers.scala.Encoding.flashsize(x.ino) + helpers.scala.Encoding.flashsize(x.part)
      case _ : types.key.dentrykey => 
        helpers.scala.Encoding.flashsize(x.ino) + helpers.scala.Encoding.flashsize(x.name)
    })
    size
  }

  def encode(x: key, buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    val newIndex = x match {
      case _ : types.key.inodekey =>
        buf(index) = 0
      var curindex = index + 1
      curindex = helpers.scala.Encoding.encode(x.ino, buf, curindex)
      curindex
      case _ : types.key.datakey =>
        buf(index) = 1
      var curindex = index + 1
      curindex = helpers.scala.Encoding.encode(x.ino, buf, curindex)
      curindex = helpers.scala.Encoding.encode(x.part, buf, curindex)
      curindex
      case _ : types.key.dentrykey =>
        buf(index) = 2
      var curindex = index + 1
      curindex = helpers.scala.Encoding.encode(x.ino, buf, curindex)
      curindex = helpers.scala.Encoding.encode(x.name, buf, curindex)
      curindex
    }
    newIndex
  }

  def decode(buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): (key, Int) = {
    import _implicit_algebraic._
    val (constructorIndex, newIndex) = helpers.scala.Encoding.decodeByte(buf, index)
    val (obj, curIndex) = constructorIndex match {
      case 0 =>
      val x0 = helpers.scala.Encoding.decodeNat(buf, newIndex)
      (types.key.inodekey(x0._1), x0._2)
      case 1 =>
      val x0 = helpers.scala.Encoding.decodeNat(buf, newIndex)
      val x1 = helpers.scala.Encoding.decodeNat(buf, x0._2)
      (types.key.datakey(x0._1, x1._1), x1._2)
      case 2 =>
      val x0 = helpers.scala.Encoding.decodeNat(buf, newIndex)
      val x1 = helpers.scala.Encoding.decodeString(buf, x0._2)
      (types.key.dentrykey(x0._1, x1._1), x1._2)
      case _ =>
        throw helpers.scala.DecodeFailure()
    }
    (obj, curIndex)
}
}
