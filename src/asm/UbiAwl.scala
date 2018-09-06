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
import types.wlstatus.wlstatus

abstract class UbiAwlInterface extends ASM {
  def bitflips(PNUM: Int)
  def check_wl(ShouldWl: Ref[Boolean])
  def format(ERR: Ref[error])
  def get_blockcount(N: Ref[Int])
  def get_ec(PNUM: Int, N: Ref[Int])
  def get_free_peb(PNUM: Ref[Int], ERR: Ref[error])
  def get_leb_for_wl(FROM: Ref[Int], AVHDR: Ref[avidheader], VALID: Ref[Boolean])
  def get_leb_size(N: Ref[Int])
  def get_page_size(N: Ref[Int])
  def get_pebs_for_wl(TO: Ref[Int], FROM: Ref[Int], VALID: Ref[Boolean], IsWl: Ref[Boolean])
  def isbad(PNUM: Int, ISBAD: Ref[Boolean], ERR: Ref[error])
  def markbad(PNUM: Int, ERR: Ref[error])
  def put_peb(PNUM: Int)
  def read_data(PNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, BITFLIPS: Ref[Boolean], ERR: Ref[error])
  def read_echdr(PNUM: Int, AEHDR: Ref[aecheader], BITFLIPS: Ref[Boolean], ERR: Ref[error])
  def read_vidhdr(PNUM: Int, AVHDR: Ref[avidheader], BITFLIPS: Ref[Boolean], ERR: Ref[error])
  def recover()
  def set_ec(PNUM: Int, N: Int)
  def set_peb_free(PNUM: Int)
  def set_status(PNUM: Int, WlStat: wlstatus)
  def sync_erase(PNUM: Int, ERR: Ref[error])
  def write_data(PNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error])
  def write_data_wl(PNUM: Int, N: Int, BUF: buffer, ERR: Ref[error])
  def write_echdr(PNUM: Int, AEHDR: aecheader, ERR: Ref[error])
  def write_vidhdr(PNUM: Int, AVHDR: avidheader, ERR: Ref[error])
}
