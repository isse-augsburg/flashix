package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error

abstract class dcache_interface {
  def delete(P_INO: Int, NAME: String, ERR: Ref[error])
  def format(ERR: Ref[error])
  def get(P_INO: Int, NAME: String, HIT: Ref[Boolean], DENT: Ref[dentry], ERR: Ref[error])
  def recovery(ERR: Ref[error])
  def set(P_INO: Int, NAME: String, DENT: dentry, ERR: Ref[error])
}
