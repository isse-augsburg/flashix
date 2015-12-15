package encoding

import helpers.scala._
import types._
import types.lpropflags.lpropflags

package object lpropflags {
  def flashsize(x: lpropflags): Int = 1

  def encode(x: lpropflags, buf: Array[Byte], index: Int): Int = {
    buf(index) = x match {
      case types.lpropflags.LP_FREE => 0
      case types.lpropflags.LP_GROUP_NODES => 1
      case types.lpropflags.LP_INDEX_NODES => 2
    }
    return index + 1
  }

  def decode(buf: Array[Byte], index: Int): (lpropflags, Int) = {
    val byte = buf(index)
    if (byte >= 3) throw helpers.scala.DecodeFailure()
    val value = byte match {
      case 0 => types.lpropflags.LP_FREE
      case 1 => types.lpropflags.LP_GROUP_NODES
      case 2 => types.lpropflags.LP_INDEX_NODES
    }
    (value, index + 1)
  }
}
