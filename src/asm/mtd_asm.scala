package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error

abstract class mtd_asm_interface {
  def erase(PNUM: Int, ERR: Ref[error])
  def get_blockcount(N: Ref[Int])
  def get_page_size(N: Ref[Int])
  def get_peb_size(N: Ref[Int])
  def init(ERR: Ref[error])
  def isbad(PNUM: Int, ISBAD: Ref[Boolean], ERR: Ref[error])
  def markbad(PNUM: Int, ERR: Ref[error])
  def read(PNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, BITFLIPS: Ref[Boolean], ERR: Ref[error])
  def write(PNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error])
}
