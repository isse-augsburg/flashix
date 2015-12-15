// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import types._
import types.error.error

abstract class AUBIIO {
  def ubi_io_format(ERR: Ref[error])
  def ubi_io_get_blockcount(N: Ref[Int])
  def ubi_io_isbad(PNUM: Int, ISBAD: Ref[Boolean], ERR: Ref[error])
  def ubi_io_markbad(PNUM: Int, ERR: Ref[error])
  def ubi_io_read_data(PNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, BITFLIPS: Ref[Boolean], ERR: Ref[error])
  def ubi_io_read_echdr(PNUM: Int, AEHDR: Ref[aecheader], BITFLIPS: Ref[Boolean], ERR: Ref[error])
  def ubi_io_read_vidhdr(PNUM: Int, AVHDR: Ref[avidheader], BITFLIPS: Ref[Boolean], ERR: Ref[error])
  def ubi_io_sync_erase(PNUM: Int, ERR: Ref[error])
  def ubi_io_write_data(PNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error])
  def ubi_io_write_data_wl(PNUM: Int, N: Int, BUF: buffer, ERR: Ref[error])
  def ubi_io_write_echdr(PNUM: Int, AEHDR: aecheader, ERR: Ref[error])
  def ubi_io_write_vidhdr(PNUM: Int, AVHDR: avidheader, ERR: Ref[error])
}
