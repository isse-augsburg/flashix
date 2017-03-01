// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error

abstract class apersistence_io_interface {
  def add_log_leb(LNUM: Int, ERR: Ref[error])
  def commit(LPT: lp_array, PROOTADR0: address, PMAXINO0: Int, ORPHANS: nat_set, ERR: Ref[error])
  def format(VOLSIZE: Int, LPT: lp_array, PROOTADR0: address, PMAXINO0: Int, ERR: Ref[error])
  def get_leb_size(N: Ref[Int])
  def get_page_size(N: Ref[Int])
  def get_volume_size(N: Ref[Int])
  def read(LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error])
  def recover(PROOTADR0: Ref[address], PMAXINO0: Ref[Int], ORPHANS: nat_set, LOG: nat_list, LPT: lp_array, ERR: Ref[error])
  def remap(LNUM: Int, ERR: Ref[error])
  def requires_commit(COMMIT_ : Ref[Boolean])
  def unmap(LNUM: Int)
  def write(LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error])
}
