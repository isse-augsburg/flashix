package encoding

import helpers.scala._
import types._

package object lprops {
  def flashsize(x: lprops)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    helpers.scala.Encoding.flashsize(x.ref_size) + helpers.scala.Encoding.flashsize(x.size) + encoding.lpropflags.flashsize(x.flags) + helpers.scala.Encoding.flashsize(x.gcheapidx)
}

  def encode(x: lprops, buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): Int = {
    import _implicit_algebraic._
    var curindex = index
    curindex = helpers.scala.Encoding.encode(x.ref_size, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.size, buf, curindex)
    curindex = encoding.lpropflags.encode(x.flags, buf, curindex)
    curindex = helpers.scala.Encoding.encode(x.gcheapidx, buf, curindex)
    curindex
  }

  def decode(buf: Array[Byte], index: Int)(implicit _implicit_algebraic: algebraic.Algebraic): (lprops, Int) = {
    import _implicit_algebraic._
    val x0 = helpers.scala.Encoding.decodeNat(buf, index)
    val x1 = helpers.scala.Encoding.decodeNat(buf, x0._2)
    val x2 = encoding.lpropflags.decode(buf, x1._2)
    val x3 = helpers.scala.Encoding.decodeNat(buf, x2._2)
    (types.lprops.mklp(x0._1, x1._1, x2._1, x3._1), x3._2)
  }
}
