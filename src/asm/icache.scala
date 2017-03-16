package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error

abstract class icache_interface {
  def check_inode(INO: Int, HIT: Ref[Boolean], DIRTY: Ref[Boolean], ERR: Ref[error])
  def delete(INO: Int, ERR: Ref[error])
  def format(ERR: Ref[error])
  def get(INO: Int, HIT: Ref[Boolean], INODE: inode, ERR: Ref[error])
  def recovery(ERR: Ref[error])
  def set(INODE: inode, DIRTY: Boolean, ERR: Ref[error])
  def set_status(INO: Int, DIRTY: Boolean, ERR: Ref[error])
}
