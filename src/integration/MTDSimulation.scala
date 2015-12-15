package integration

import java.io._
import helpers.scala._
import types._
import types.error._
import java.util.Arrays

class MTDSimulation private(val file: RandomAccessFile, val PEBS: Int, val PAGES_PER_PEB: Int, val EB_PAGE_SIZE: Int) extends asm.MTD {

  val empty: Byte = 0xFF.toByte
  val PEB_SIZE: Int = PAGES_PER_PEB * EB_PAGE_SIZE

  final override def mtd_erase(PNUM: Int, ERR: Ref[error]) {
    val empty_peb = new Array[Byte](PEB_SIZE)
    (0 until PEB_SIZE).foreach { i =>
      empty_peb(i) = empty
    }
    try {
      file.seek(PNUM * PEB_SIZE)
      file.write(empty_peb)
      ERR := ESUCCESS
    } catch {
      case e: Exception =>
        println(s"MTDSimulation::erase: failed with exception ${e}: ${e.getMessage}")
        e.printStackTrace()
        ERR := EIO
    }
  }

  final override def mtd_get_blockcount(N: Ref[Int]) {
    N := PEBS
  }

  final override def mtd_isbad(PNUM: Int, ISBAD: Ref[Boolean], ERR: Ref[error]) {
    ISBAD := false
    ERR := ESUCCESS
  }

  final override def mtd_markbad(PNUM: Int, ERR: Ref[error]) {
    println("MTDSimulation: markbad not supported")
    ERR := EIO
  }

  final override def mtd_read(PNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, BITFLIPS: Ref[Boolean], ERR: Ref[error]) {
    try {
      file.seek(PNUM * PEB_SIZE + OFFSET)
      file.read(BUF.array, N0, N)
      ERR := ESUCCESS
    } catch {
      case e: Exception =>
        println(s"MTDSimulation::read: failed with exception ${e}: ${e.getMessage}")
        e.printStackTrace()
        ERR := EIO
    }
    BITFLIPS := false
  }

  final override def mtd_write(PNUM: Int, OFFSET: Int, N0: Int, N: Int, BUF: buffer, ERR: Ref[error]) {
    try {
      file.seek(PNUM * PEB_SIZE + OFFSET)
      file.write(BUF.array, N0, N)
      ERR := ESUCCESS
    } catch {
      case e: Exception =>
        println(s"MTDSimulation::write: failed with exception ${e}: ${e.getMessage}")
        e.printStackTrace()
        ERR := EIO
    }
  }

  def close {
    file.close()
  }
}

object MTDSimulation {
  def apply(filename: File, pebs: Int, pages_per_peb: Int, page_size: Int): MTDSimulation = {
    if (filename.exists() && filename.length() != pebs * pages_per_peb * page_size)
      throw new IOException("MTDSimulation: file size does not match device characteristics")
    val file = new RandomAccessFile(filename, "rw")
    file.setLength(pebs * pages_per_peb * page_size)
    new MTDSimulation(file, pebs, pages_per_peb, page_size)
  }
}
