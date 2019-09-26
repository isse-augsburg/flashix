// Flashix: a verified file system for flash memory
// (c) 2015-2019 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._
import types._
import types.error.error

abstract class AwbufInterface extends ASM {
  def add_log_leb(LNUM: Int, OFFSET: Int, ERR: Ref[error])
  def commit(LPT: lp_array, WROOTADR0: address, WMAXINO0: Int, ORPHANS: nat_set, ERR: Ref[error])
  def destroy_buf(ERR: Ref[error])
  def enter_readonly()
  def format(VOLSIZE: Int, SIZE: Int, LPT: lp_array, WROOTADR0: address, WMAXINO0: Int, ERR: Ref[error])
  def get_buf(WBUFLEB0: Ref[bufleb])
  def get_leb_size(N: Ref[Int])
  def get_page_size(N: Ref[Int])
  def is_readonly(AROFS0: Ref[Boolean])
  def move_buf(LNUM: Int, OFFSET: Int, ERR: Ref[error])
  def read_buf(LNUM: Int, OFFSET: Int, N: Int, BUF: buffer, ERR: Ref[error])
  def recover(WROOTADR0: Ref[address], WMAXINO0: Ref[Int], ORPHANS: nat_set, LOG: nat_list, LPT: lp_array, ERR: Ref[error])
  def remap(LNUM: Int, ERR: Ref[error])
  def requires_commit(COMMIT_ : Ref[Boolean])
  def unmap(LNUM: Int)
  def write_buf(N: Int, BUF: buffer, ERR: Ref[error])
}
