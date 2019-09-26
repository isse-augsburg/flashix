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

abstract class AubiIoInterface extends ASM {
  def format(Err: Ref[error])
  def get_blockcount(N: Ref[Int])
  def get_leb_size(N: Ref[Int])
  def get_page_size(N: Ref[Int])
  def isbad(Pnum: Int, Isbad: Ref[Boolean], Err: Ref[error])
  def markbad(Pnum: Int, Err: Ref[error])
  def read_data(Pnum: Int, Offset: Int, N0: Int, N: Int, Buf: buffer, Bitflips: Ref[Boolean], Err: Ref[error])
  def read_echdr(Pnum: Int, Aehdr: Ref[aecheader], Bitflips: Ref[Boolean], Err: Ref[error])
  def read_vidhdr(Pnum: Int, Avhdr: Ref[avidheader], Bitflips: Ref[Boolean], Err: Ref[error])
  def recovery()
  def sync_erase(Pnum: Int, Err: Ref[error])
  def write_data(Pnum: Int, Offset: Int, N0: Int, N: Int, Buf: buffer, Err: Ref[error])
  def write_data_wl(Pnum: Int, N: Int, Buf: buffer, Err: Ref[error])
  def write_echdr(Pnum: Int, Aehdr: aecheader, Err: Ref[error])
  def write_vidhdr(Pnum: Int, Avhdr: avidheader, Err: Ref[error])
}
