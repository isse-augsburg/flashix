// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package encoding

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import sorts._
import types._
import types.error.error
import types.file_mode.file_mode
import types.lpropflags.lpropflags
import types.seekflag.seekflag
import types.wlstatus.wlstatus

object lpropflags {
  def ENCODED_LPROPFLAGS_SIZE(implicit _algebraic_implicit: algebraic.Algebraic): Int = {
    return 1
  }

  def encode_lpropflags(elem: lpropflags, index: Int, buf: buffer, nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    nbytes := 1
    err := types.error.ESUCCESS
    if (elem == types.lpropflags.LP_FREE)
      buf(index) = 0
    else     if (elem == types.lpropflags.LP_GROUP_NODES)
      buf(index) = 1
    else     if (elem == types.lpropflags.LP_INDEX_NODES)
      buf(index) = 2
    else
      assert(false)
  }

  def decode_lpropflags(index: Int, buf: buffer, elem: Ref[lpropflags], nbytes: Ref[Int], err: Ref[error])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    nbytes := 1
    err := types.error.ESUCCESS
    if (buf(index) == 0)
      elem := types.lpropflags.LP_FREE
    else     if (buf(index) == 1)
      elem := types.lpropflags.LP_GROUP_NODES
    else     if (buf(index) == 2)
      elem := types.lpropflags.LP_INDEX_NODES
    else
      err := types.error.EINVAL
  }
}
