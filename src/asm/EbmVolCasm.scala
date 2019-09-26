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

class EbmVolCasm(var VOLID : Byte, val ebm : EbmInterface)(implicit _algebraic_implicit: algebraic.Algebraic) extends EbmAvolInterface {
  import _algebraic_implicit._

  override def change(LNUM: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    ebm.change(VOLID, LNUM, N, BUF, ERR)
  }

  override def format(VOLSIZE: Int, ERR: Ref[error]): Unit = {
    ebm.format(default_volid, VOLSIZE, ERR)
    if (ERR.get == types.error.ESUCCESS) {
      VOLID = default_volid
    }
  }

  override def get_leb_size(N: Ref[Int]): Unit = {
    ebm.get_leb_size(N)
  }

  override def get_page_size(N: Ref[Int]): Unit = {
    ebm.get_page_size(N)
  }

  override def get_volume_size(N: Ref[Int]): Unit = {
    ebm.get_volume_size(VOLID, N)
  }

  override def read(LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    ebm.read(VOLID, LNUM, OFFSET, N0, N, BUF, ERR)
  }

  override def recover(ERR: Ref[error]): Unit = {
    ebm.recover(ERR)
    VOLID = default_volid
  }

  override def remap(LNUM: Int, ERR: Ref[error]): Unit = {
    unmap(LNUM)
    ebm.map(VOLID, LNUM, ERR)
  }

  override def sync_device(ERR: Ref[error]): Unit = {
    ebm.sync_device(ERR)
  }

  override def unmap(LNUM: Int): Unit = {
    ebm.unmap(VOLID, LNUM)
  }

  override def write(LNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error]): Unit = {
    ebm.write(VOLID, LNUM, OFFSET, N0, N, BUF, ERR)
  }

}
