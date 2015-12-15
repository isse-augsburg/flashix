package asm

import helpers.scala._
import sorts._
import types._
import types.error.error
import types.file_mode.file_mode
import types.seekflag.seekflag

abstract class POSIX {
  def posix_close(FD: Int, USER: user, ERR: Ref[error])
  def posix_create(PATH: path, MD: metadata, USER: user, ERR: Ref[error])
  def posix_format(N: Int, MD: metadata, ERR: Ref[error])
  def posix_link(PATH: path, PATH_ : path, USER: user, ERR: Ref[error])
  def posix_mkdir(PATH: path, MD: metadata, USER: user, ERR: Ref[error])
  def posix_open(PATH: path, MODE0: file_mode, USER: user, FD: Ref[Int], ERR: Ref[error])
  def posix_read(FD: Int, USER: user, N: Ref[Int], BUF: buffer, ERR: Ref[error])
  def posix_readdir(PATH: path, USER: user, NAMES: stringset, ERR: Ref[error])
  def posix_readmeta(PATH: path, USER: user, MD: Ref[metadata], NLINK: Ref[Int], SIZE: Ref[Int], ERR: Ref[error])
  def posix_rename(PATH: path, PATH_ : path, USER: user, ERR: Ref[error])
  def posix_rmdir(PATH: path, USER: user, ERR: Ref[error])
  def posix_seek(FD: Int, WHENCE: seekflag, USER: user, N: Ref[Int], ERR: Ref[error])
  def posix_truncate(PATH: path, N: Int, USER: user, ERR: Ref[error])
  def posix_unlink(PATH: path, USER: user, ERR: Ref[error])
  def posix_write(FD: Int, BUF: buffer, USER: user, N: Ref[Int], ERR: Ref[error])
  def posix_writemeta(PATH: path, MD: metadata, USER: user, ERR: Ref[error])
}
