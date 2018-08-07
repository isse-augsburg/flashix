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
import types.file_mode.file_mode
import types.seekflag.seekflag

abstract class PosixInterface extends ASM {
  def close(FD: Int, USER: Byte, ERR: Ref[error])
  def create(PATH: path, MD: metadata, USER: Byte, ERR: Ref[error])
  def format(N: Int, DOSYNC: Boolean, SIZE: Int, MD: metadata, ERR: Ref[error])
  def fsync(FD: Int, ISDATASYNC: Boolean, USER: Byte, ERR: Ref[error])
  def fsyncdir(PATH: path, ISDATASYNC: Boolean, USER: Byte, ERR: Ref[error])
  def link(PATH: path, PATH_ : path, USER: Byte, ERR: Ref[error])
  def mkdir(PATH: path, MD: metadata, USER: Byte, ERR: Ref[error])
  def open(PATH: path, MODE: file_mode, USER: Byte, FD: Ref[Int], ERR: Ref[error])
  def read(FD: Int, USER: Byte, BUF: buffer, N: Ref[Int], ERR: Ref[error])
  def readdir(PATH: path, USER: Byte, NAMES: stringset, ERR: Ref[error])
  def readmeta(PATH: path, USER: Byte, MD: Ref[metadata], NLINK: Ref[Int], SIZE: Ref[Int], ERR: Ref[error])
  def recover(DOSYNC: Boolean, ERR: Ref[error])
  def rename(PATH: path, PATH_ : path, USER: Byte, ERR: Ref[error])
  def rmdir(PATH: path, USER: Byte, ERR: Ref[error])
  def seek(FD: Int, WHENCE: seekflag, USER: Byte, N: Ref[Int], ERR: Ref[error])
  def sync(ERR: Ref[error])
  def truncate(PATH: path, N: Int, USER: Byte, ERR: Ref[error])
  def unlink(PATH: path, USER: Byte, ERR: Ref[error])
  def write(FD: Int, BUF: buffer, USER: Byte, N: Ref[Int], ERR: Ref[error])
  def writemeta(PATH: path, MD: metadata, USER: Byte, ERR: Ref[error])
}
