package encoding

import helpers.scala._
import types._

package object node {
  def flashsize(x: node)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    val size = 1 + (x match {
      case _ : types.node.inodenode => 
        encoding.key.flashsize(x.key) + encoding.metadata.flashsize(x.meta) + helpers.scala.Encoding.flashsize(x.directory) + helpers.scala.Encoding.flashsize(x.nlink) + helpers.scala.Encoding.flashsize(x.size)
      case _ : types.node.dentrynode => 
        encoding.key.flashsize(x.key) + helpers.scala.Encoding.flashsize(x.ino)
      case _ : types.node.datanode => 
        encoding.key.flashsize(x.key) + (helpers.scala.Encoding.flashsizeArrayWrapper[Byte](_: buffer, helpers.scala.Encoding.flashsize))(x.data)
      case _ : types.node.truncnode => 
        encoding.key.flashsize(x.key) + helpers.scala.Encoding.flashsize(x.size)
    })
    size
  }

  def encode(x: node, buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    val newIndex = x match {
      case _ : types.node.inodenode =>
        buf(index) = 0
      var curindex = index + 1
      curindex = encoding.key.encode(x.key, buf, curindex)
      curindex = encoding.metadata.encode(x.meta, buf, curindex)
      curindex = helpers.scala.Encoding.encode(x.directory, buf, curindex)
      curindex = helpers.scala.Encoding.encode(x.nlink, buf, curindex)
      curindex = helpers.scala.Encoding.encode(x.size, buf, curindex)
      curindex
      case _ : types.node.dentrynode =>
        buf(index) = 1
      var curindex = index + 1
      curindex = encoding.key.encode(x.key, buf, curindex)
      curindex = helpers.scala.Encoding.encode(x.ino, buf, curindex)
      curindex
      case _ : types.node.datanode =>
        buf(index) = 2
      var curindex = index + 1
      curindex = encoding.key.encode(x.key, buf, curindex)
      curindex = (helpers.scala.Encoding.encodeArrayWrapper[Byte](_: buffer, _: Array[Byte], _: Int, helpers.scala.Encoding.encode))(x.data, buf, curindex)
      curindex
      case _ : types.node.truncnode =>
        buf(index) = 3
      var curindex = index + 1
      curindex = encoding.key.encode(x.key, buf, curindex)
      curindex = helpers.scala.Encoding.encode(x.size, buf, curindex)
      curindex
    }
    newIndex
  }

  def decode(buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): (node, Int) = {
    import _implicit_algebraic._
    val (constructorIndex, newIndex) = helpers.scala.Encoding.decodeByte(buf, index)
    val (obj, curIndex) = constructorIndex match {
      case 0 =>
      val x0 = encoding.key.decode(buf, newIndex)
      val x1 = encoding.metadata.decode(buf, x0._2)
      val x2 = helpers.scala.Encoding.decodeBoolean(buf, x1._2)
      val x3 = helpers.scala.Encoding.decodeNat(buf, x2._2)
      val x4 = helpers.scala.Encoding.decodeNat(buf, x3._2)
      (types.node.inodenode(x0._1, x1._1, x2._1, x3._1, x4._1), x4._2)
      case 1 =>
      val x0 = encoding.key.decode(buf, newIndex)
      val x1 = helpers.scala.Encoding.decodeNat(buf, x0._2)
      (types.node.dentrynode(x0._1, x1._1), x1._2)
      case 2 =>
      val x0 = encoding.key.decode(buf, newIndex)
      val x1 = (helpers.scala.Encoding.decodeArrayWrapper[Byte](_: Array[Byte], _: Int, helpers.scala.Encoding.decodeByte))(buf, x0._2)
      (types.node.datanode(x0._1, x1._1), x1._2)
      case 3 =>
      val x0 = encoding.key.decode(buf, newIndex)
      val x1 = helpers.scala.Encoding.decodeNat(buf, x0._2)
      (types.node.truncnode(x0._1, x1._1), x1._2)
      case _ =>
        throw helpers.scala.DecodeFailure()
    }
    (obj, curIndex)
}
}
