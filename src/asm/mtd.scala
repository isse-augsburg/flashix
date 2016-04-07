// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error

abstract class mtd_interface {
  def mtd_erase(PNUM: Int, ERR: Ref[error])
  def mtd_get_blockcount(N: Ref[Int])
  def mtd_init(ERR: Ref[error])
  def mtd_isbad(PNUM: Int, ISBAD: Ref[Boolean], ERR: Ref[error])
  def mtd_markbad(PNUM: Int, ERR: Ref[error])
  def mtd_read(PNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, BITFLIPS: Ref[Boolean], ERR: Ref[error])
  def mtd_write(PNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error])
}