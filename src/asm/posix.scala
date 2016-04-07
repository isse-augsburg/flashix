// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._
import types.error.error
import types.file_mode.file_mode
import types.seekflag.seekflag

abstract class posix_interface {
  def posix_close(FD: Int, USER: Byte, ERR: Ref[error])
  def posix_create(PATH: path, MD: metadata, USER: Byte, ERR: Ref[error])
  def posix_format(N: Int, MD: metadata, ERR: Ref[error])
  def posix_link(PATH: path, PATH_ : path, USER: Byte, ERR: Ref[error])
  def posix_mkdir(PATH: path, MD: metadata, USER: Byte, ERR: Ref[error])
  def posix_open(PATH: path, MODE: file_mode, USER: Byte, FD: Ref[Int], ERR: Ref[error])
  def posix_read(FD: Int, USER: Byte, BUF: buffer, N: Ref[Int], ERR: Ref[error])
  def posix_readdir(PATH: path, USER: Byte, NAMES: stringset, ERR: Ref[error])
  def posix_readmeta(PATH: path, USER: Byte, MD: Ref[metadata], NLINK: Ref[Int], SIZE: Ref[Int], ERR: Ref[error])
  def posix_recover(ERR: Ref[error])
  def posix_rename(PATH: path, PATH_ : path, USER: Byte, ERR: Ref[error])
  def posix_rmdir(PATH: path, USER: Byte, ERR: Ref[error])
  def posix_seek(FD: Int, WHENCE: seekflag, USER: Byte, N: Ref[Int], ERR: Ref[error])
  def posix_truncate(PATH: path, N: Int, USER: Byte, ERR: Ref[error])
  def posix_unlink(PATH: path, USER: Byte, ERR: Ref[error])
  def posix_write(FD: Int, BUF: buffer, USER: Byte, N: Ref[Int], ERR: Ref[error])
  def posix_writemeta(PATH: path, MD: metadata, USER: Byte, ERR: Ref[error])
}
