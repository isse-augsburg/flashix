package test

import java.io._
import java.util.UUID

import helpers.scala.Random

import scala.collection.mutable

object TestGC {
  def main(initialArgs: Array[String]) {
    if (initialArgs.length != 3) {
      printHelp()
      System.exit(1)
    }

    val mountPoint = initialArgs(initialArgs.length - 3)
    val pageSize = initialArgs(initialArgs.length - 2).toInt
    val maxFilePages = initialArgs(initialArgs.length - 1).toInt
    if (pageSize <= 0 || maxFilePages <= 0) {
      printHelp()
      System.exit(1)
    }
    val t = new TestGC(mountPoint, pageSize, maxFilePages)

    t.run()
  }

  def printHelp() {
    println("usage:")
    println("  testGC <mountpoint> <page_size> <maxPagesPerFile>")
  }
}

/**
  * Tests that all files are written completely to the filesystem and are readable later
  *
  * Does that by repeatedly creating, overwriting, appending and deleting
  * files with random sizes.
  * When started the thread will run infinitely until a file error is detected
  * or the thread is stopped manually.
  * If possible all created files will be cleaned up at the end.
  *
  * Warning:
  * - may overwrite existing files
  * - may make the filesystem unusable by completely filling it
  * @param mountPoint path, where the flashix filesystem is mounted
  * @param pageSize the page size in the flashix filesystem
  * @param maxFilePages the maximum number of pages a created file should have,
  *                     larger files may be created in the process by appending
  *                     to existing ones
  */
class TestGC(mountPoint: String, pageSize: Int, maxFilePages: Int) extends Runnable {
  private val maxFileSize = maxFilePages * pageSize // maximum file size in bytes
  private val files = new mutable.HashMap[UUID, Int]()
  private val getRandInt = Random.IntRandomizer
  var running = false
  private var i = 0

  /**
    * Start random filesystem operations infinitely until told to stop or an error occurred
    */
  def run(): Unit = {
    running = true

    while (running) {
      try {
        // select a random filesystem operation
        Math.abs(getRandInt.random() % 6) match {
          case 0 => if (files.nonEmpty) removeFile(files.keySet.head)
          case 1 => if (files.nonEmpty) appendToFile(files.keySet.head)
          case 2 => if (files.nonEmpty) overwriteFile(files.keySet.head)
          case _ => newFile()
        }
        i += 1

        // regularly check all files for completeness
        if (i % 256 == 0)
          checkAllFiles()
      } catch {
        // device may be full: remove some files and continue
        case e: IOException =>
          e.printStackTrace()
          println("Filesystem is full. Deleting some files...")
          for (_ <- 0 to files.keySet.size / 4)
            removeFile(files.keySet.last)

        // a definitely invalid file has been found: terminate and cleanup
        // other exceptions occurred: do the same
        case e@(_: InvalidObjectException | _: Exception) =>
          e.printStackTrace()
          cleanup()
          running = false
      }
    }

    cleanup()
  }

  private def cleanup(): Unit = {
    println("Cleaning up all created files...")
    for ((f, _) <- files) {
      println(s"Cleanup: Deleting $f")
      new File(s"$mountPoint/$f").delete()
      files -= f
    }
  }

  private def newFile(): Unit = {
    // filename
    val (name, buf) = createFileMetadata()
    println("Creating file %s with length %d" format(name, buf.length))

    val out = new FileOutputStream(s"$mountPoint/$name")
    out.write(buf)
    out.close()
    files(name) = buf.length
  }

  private def createFileMetadata(): (UUID, Array[Byte]) = {
    val f = UUID.randomUUID()

    (f, createBuffer())
  }

  private def createBuffer(): Array[Byte] = {
    val length = if (Random.BooleanRandomizer.random()) {
      // file size may not fit exactly into page borders
      getRandInt.random().abs % maxFileSize
    } else {
      // file size is multiple of page size
      (getRandInt.random().abs % maxFilePages) * pageSize
    }

    new Array[Byte](length)
  }

  private def appendToFile(uuid: UUID): Unit = {
    if (files.isEmpty)
      return

    val appendBuffer = createBuffer()
    val newLength = files(uuid) + appendBuffer.length
    println("Appending %d Bytes to %s. New length is %d" format(appendBuffer.length, uuid, newLength))

    val f = new FileOutputStream(s"$mountPoint/$uuid", true)
    f.write(appendBuffer)
    f.close()

    files(uuid) = newLength
  }

  private def overwriteFile(uuid: UUID): Unit = {
    if (files.isEmpty)
      return

    val newBuffer = createBuffer()
    println("Overwriting file %s. New length is %d" format(uuid, newBuffer.length))

    val f = new FileOutputStream(s"$mountPoint/$uuid", false)
    f.write(newBuffer)
    f.close()

    files(uuid) = newBuffer.length
  }

  private def removeFile(uuid: UUID, checkSize: Boolean = true): Unit = {
    if (files.isEmpty)
      return

    println(s"Deleting file $uuid")

    val f = new File(s"$mountPoint/$uuid")

    // if the device is full the size might not have been written completely,
    // so skip the check
    if (checkSize) {
      val size = files(uuid)
      val reader = new FileInputStream(f)
      val bytesRead = reader.read(new Array(size))

      if (f.length() != size || bytesRead != size) {
        throw new InvalidObjectException(s"Actual size of file $uuid is $bytesRead, but expected it to be $size!")
      }
      reader.close()
    }

    f.delete()

    files -= uuid
  }

  /**
    * Check all files for completeness by reading their content and checking the length
    */
  private def checkAllFiles(): Unit = {
    println("Checking all files for completeness... ")

    for ((uuid, size) <- files) {
      val f = new File(s"$mountPoint/$uuid")
      val reader = new FileInputStream(f)
      val bytesRead = reader.read(new Array(size))

      // check that the file length reported by the OS and the readable bytes
      // matches the written size
      if (f.length() != size || bytesRead != size) {
        throw new InvalidObjectException(s"Actual size of file $uuid is $bytesRead, but expected it to be $size!")
      }

      reader.close()
    }

    println("Check OK")
  }

  def stop(): Unit = {
    running = false
  }
}