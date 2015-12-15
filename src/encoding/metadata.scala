// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package encoding

import helpers.scala._
import types._

package object metadata {
  def flashsize(x: metadata)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    helpers.scala.Encoding.flashsize(x.mode) + helpers.scala.Encoding.flashsize(x.uid) + helpers.scala.Encoding.flashsize(x.gid) + helpers.scala.Encoding.flashsize(x.atime) + helpers.scala.Encoding.flashsize(x.atimesec) + helpers.scala.Encoding.flashsize(x.mtime) + helpers.scala.Encoding.flashsize(x.mtimesec) + helpers.scala.Encoding.flashsize(x.ctime) + helpers.scala.Encoding.flashsize(x.ctimesec)
}

  def encode(x: metadata, buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    var curindex = index
    curindex = helpers.scala.Encoding.encode(x.mode, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.uid, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.gid, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.atime, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.atimesec, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.mtime, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.mtimesec, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.ctime, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.ctimesec, buf, curindex)
    curindex
  }

  def decode(buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): (metadata, Int) = {
    import _implicit_algebraic._
    val x0 = helpers.scala.Encoding.decodeNat(buf, index)
    val x1 = helpers.scala.Encoding.decodeNat(buf, x0._2)
    val x2 = helpers.scala.Encoding.decodeNat(buf, x1._2)
    val x3 = helpers.scala.Encoding.decodeNat(buf, x2._2)
    val x4 = helpers.scala.Encoding.decodeNat(buf, x3._2)
    val x5 = helpers.scala.Encoding.decodeNat(buf, x4._2)
    val x6 = helpers.scala.Encoding.decodeNat(buf, x5._2)
    val x7 = helpers.scala.Encoding.decodeNat(buf, x6._2)
    val x8 = helpers.scala.Encoding.decodeNat(buf, x7._2)
    (types.metadata.mkmetadata(x0._1, x1._1, x2._1, x3._1, x4._1, x5._1, x6._1, x7._1, x8._1), x8._2)
  }
}
