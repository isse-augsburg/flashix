// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error

abstract class awbuf_interface {
  def awbuf_change(LNUM: Int, N: Int, BUF: buffer, ERR: Ref[error])
  def awbuf_create_buf(LNUM: Int, OFFSET: Int)
  def awbuf_destroy_buf(LNUM: Int)
  def awbuf_destroy_bufs()
  def awbuf_format(VOLSIZE: Int, ERR: Ref[error])
  def awbuf_get_bufs(WBS0: nat_set)
  def awbuf_get_volume_size(N: Ref[Int])
  def awbuf_is_buffered(LNUM: Int, ISBUF: Ref[Boolean])
  def awbuf_read(LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error])
  def awbuf_read_buf(LNUM: Int, OFFSET: Int, N: Int, BUF: buffer, ERR: Ref[error])
  def awbuf_recover(ERR: Ref[error])
  def awbuf_remap(LNUM: Int, ERR: Ref[error])
  def awbuf_unmap(LNUM: Int)
  def awbuf_write(LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error])
  def awbuf_write_buf(LNUM: Int, N: Int, BUF: buffer, ERR: Ref[error])
}
