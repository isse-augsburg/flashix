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

abstract class EbmSeqInterface extends ASM {
  def change(VOLID: Byte, LNUM: Int, N: Int, BUF: buffer, ERR: Ref[error])
  def create_volume(VOLID: Byte, N: Int, ERR: Ref[error])
  def erase(VOLID: Byte, LNUM: Int, ERR: Ref[error])
  def format(ERR: Ref[error])
  def get_leb_size(N: Ref[Int])
  def get_page_size(N: Ref[Int])
  def get_volume_size(VOLID: Byte, N: Ref[Int])
  def map(VOLID: Byte, LNUM: Int, ERR: Ref[error])
  def read(VOLID: Byte, LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error])
  def recover(ERR: Ref[error])
  def sync_device(ERR: Ref[error])
  def unmap(VOLID: Byte, LNUM: Int)
  def write(VOLID: Byte, LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error])
}
