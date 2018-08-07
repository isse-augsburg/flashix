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

abstract class EbmAvolInterface extends ASM {
  def change(LNUM: Int, N: Int, BUF: buffer, ERR: Ref[error])
  def format(VOLSIZE: Int, ERR: Ref[error])
  def get_leb_size(N: Ref[Int])
  def get_page_size(N: Ref[Int])
  def get_volume_size(N: Ref[Int])
  def read(LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error])
  def recover(ERR: Ref[error])
  def remap(LNUM: Int, ERR: Ref[error])
  def sync_device(ERR: Ref[error])
  def unmap(LNUM: Int)
  def write(LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error])
}
