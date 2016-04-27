// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error

abstract class ebm_interface {
  def ebm_change(VOLID: Byte, LNUM: Int, N: Int, BUF: buffer, ERR: Ref[error])
  def ebm_create_volume(VOLID: Byte, N: Int, ERR: Ref[error])
  def ebm_erase(VOLID: Byte, LNUM: Int, ERR: Ref[error])
  def ebm_format(ERR: Ref[error])
  def ebm_get_volume_size(VOLID: Byte, N: Ref[Int])
  def ebm_map(VOLID: Byte, LNUM: Int, ERR: Ref[error])
  def ebm_read(VOLID: Byte, LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error])
  def ebm_recover(ERR: Ref[error])
  def ebm_sync_device(ERR: Ref[error])
  def ebm_unmap(VOLID: Byte, LNUM: Int)
  def ebm_write(VOLID: Byte, LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error])
}
