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

abstract class AubiSioInterface extends ASM {
  def format(ERR: Ref[error])
  def get_blockcount(N: Ref[Int])
  def get_leb_size(N: Ref[Int])
  def get_page_size(N: Ref[Int])
  def isbad(PNUM: Int, ISBAD: Ref[Boolean], ERR: Ref[error])
  def markbad(PNUM: Int, ERR: Ref[error])
  def read_data(PNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, BITFLIPS: Ref[Boolean], ERR: Ref[error])
  def read_echdr(PNUM: Int, AEHDR: Ref[aecheader], BITFLIPS: Ref[Boolean], ERR: Ref[error])
  def read_vidhdr(PNUM: Int, AVHDR: Ref[avidheader], BITFLIPS: Ref[Boolean], ERR: Ref[error])
  def recovery()
  def sync_erase(PNUM: Int, ERR: Ref[error])
  def write_data(PNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error])
  def write_data_wl(PNUM: Int, N: Int, BUF: buffer, ERR: Ref[error])
  def write_echdr(PNUM: Int, AEHDR: aecheader, ERR: Ref[error])
  def write_vidhdr(PNUM: Int, AVHDR: avidheader, ERR: Ref[error])
}
