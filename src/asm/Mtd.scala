// Flashix: a verified file system for flash memory
// (c) 2015-2018 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._
import types._
import types.error.error

abstract class MtdInterface extends ASM {
  def erase(Pnum: Int, Err: Ref[error])
  def get_blockcount(N: Ref[Int])
  def get_page_size(N: Ref[Int])
  def get_peb_size(N: Ref[Int])
  def init(Err: Ref[error])
  def isbad(Pnum: Int, Isbad: Ref[Boolean], Err: Ref[error])
  def markbad(Pnum: Int, Err: Ref[error])
  def read(Pnum: Int, Offset: Int, N0: Int, N: Int, Buf: buffer, Bitflips: Ref[Boolean], Err: Ref[error])
  def write(Pnum: Int, Offset: Int, N0: Int, N: Int, Buf: buffer, Err: Ref[error])
}
