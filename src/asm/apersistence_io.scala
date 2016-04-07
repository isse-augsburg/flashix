// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error

abstract class apersistence_io_interface {
  def apersistence_io_add_log_leb(LNUM: Int, ERR: Ref[error])
  def apersistence_io_commit(LPT: lp_array, PROOTADR0: address, PMAXINO0: Int, ORPHANS: nat_set, ERR: Ref[error])
  def apersistence_io_create_buf(LNUM: Int, OFFSET: Int)
  def apersistence_io_destroy_buf(LNUM: Int)
  def apersistence_io_destroy_bufs()
  def apersistence_io_format(VOLSIZE: Int, LPT: lp_array, PROOTADR0: address, PMAXINO0: Int, ERR: Ref[error])
  def apersistence_io_get_bufs(PWBS0: nat_set)
  def apersistence_io_get_volume_size(N: Ref[Int])
  def apersistence_io_is_buffered(LNUM: Int, ISBUF: Ref[Boolean])
  def apersistence_io_read_buf(LNUM: Int, OFFSET: Int, N: Int, BUF: buffer, ERR: Ref[error])
  def apersistence_io_recover(PROOTADR0: Ref[address], PMAXINO0: Ref[Int], ORPHANS: nat_set, LOG: nat_list, LPT: lp_array, ERR: Ref[error])
  def apersistence_io_remap(LNUM: Int, ERR: Ref[error])
  def apersistence_io_requires_commit(COMMIT_ : Ref[Boolean])
  def apersistence_io_unmap(LNUM: Int)
  def apersistence_io_write_buf(LNUM: Int, N: Int, BUF: buffer, ERR: Ref[error])
}
