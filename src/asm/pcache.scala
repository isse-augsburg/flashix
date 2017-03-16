package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error

abstract class pcache_interface {
  def check_page(INO: Int, PAGENO: Int, HIT: Ref[Boolean], DIRTY: Ref[Boolean], ERR: Ref[error])
  def delete(INO: Int, PAGENO: Int, ERR: Ref[error])
  def evict(INO: Int, ERR: Ref[error])
  def format(ERR: Ref[error])
  def get(INO: Int, PAGENO: Int, HIT: Ref[Boolean], BUF: buffer, ERR: Ref[error])
  def recovery(ERR: Ref[error])
  def set(INO: Int, PAGENO: Int, BUF: buffer, DIRTY: Boolean, ERR: Ref[error])
  def set_status(INO: Int, PAGENO: Int, DIRTY: Boolean, ERR: Ref[error])
  def truncate(INO: Int, SIZE: Int, N: Int, ERR: Ref[error])
}
