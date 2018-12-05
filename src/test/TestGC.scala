// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package test

import java.io._
import java.util.UUID

import helpers.scala.Random

import scala.collection.mutable

object TestGC {
  def main(initialArgs: Array[String]) {
    if (initialArgs.length != 4) {
      printHelp()
      System.exit(1)
    }

    val mountPoint = initialArgs(initialArgs.length - 4)
    val pageSize = initialArgs(initialArgs.length - 3).toInt
    val maxFilePages = initialArgs(initialArgs.length - 2).toInt
    val timeout = initialArgs(initialArgs.length - 1).toInt
    if (pageSize <= 0 || maxFilePages <= 0) {
      printHelp()
      System.exit(1)
    }
    val t = new TestGC(mountPoint, pageSize, maxFilePages, timeout)

    t.run()
  }

  def printHelp() {
    println("usage:")
    println("  testGC <mountpoint> <page_size> <maxPagesPerFile> <timeout>")
    println("  timeout: in seconds; <= 0 to run the test indefinitely")
  }
}

/**
  * Tests that all files are written completely to the filesystem and are readable later
  *
  * Does that by repeatedly creating, overwriting, appending and deleting
  * files with random sizes.
  * When started the thread will run infinitely until a file error is detected
  * or the thread is stopped manually.
  * If possible all created files will be cleaned up at the end (some might
  * persist because a cleaning was not possible with a completely full device).
  *
  * Warning:
  * - may overwrite existing files
  * - may make the filesystem unusable by completely filling it
  *
  * @param mountPoint   path, where the flashix filesystem is mounted
  * @param pageSize     the page size in the flashix filesystem
  * @param maxFilePages the maximum number of pages a created file should have,
  *                     larger files may be created in the process by appending
  *                     to existing ones
  * @param timeout      if this value is > 0 then the test runs only for timeout seconds
  */
class TestGC(mountPoint: String, pageSize: Int, maxFilePages: Int, timeout: Int = 0) extends Runnable {
  private val maxFileSize = maxFilePages * pageSize // maximum file size in bytes
  private val files = new mutable.HashMap[UUID, Int]()
  private val getRandInt = Random.IntRandomizer
  private var running = false
  private var i = 0

  /**
    * Start random filesystem operations infinitely until told to stop or an error occurred
    */
  def run(): Unit = {
    running = true
    val startTime = System.currentTimeMillis()

    while (running) {
      try {
        // select a random filesystem operation
        Math.abs(getRandInt.random() % 8) match {
          case 0 => if (files.nonEmpty) removeFile(files.keySet.head)
          case 1 => if (files.nonEmpty) appendToFile(files.keySet.head)
          case 2 => if (files.nonEmpty) overwriteFile(files.keySet.head)
          case _ => newFile()
        }
        i += 1

        // regularly check all files for completeness
        if (i % 128 == 0)
          checkAllFiles()
      } catch {
        // a definitely invalid file has been found: terminate and cleanup
        case e: InvalidObjectException =>
          e.printStackTrace()
          running = false

        // device may be full: remove some files and continue
        case e: IOException =>
          e.printStackTrace()
          partialCleanup()

        // received an interrupt => stop running, but still do the cleanup
        case _: InterruptedException => running = false

        // other unexpected exceptions occurred: terminate after cleanup
        case e: Exception =>
          e.printStackTrace()
          running = false
      } finally {
        // always check if maximum running time is reached
        if (timeout > 0 && System.currentTimeMillis() - startTime > timeout * 1000) {
          println("Finished test-process after timeout! No errors detected!")
          running = false
        }
      }
    }

    cleanup()
  }

  def stop(): Unit = {
    running = false
  }

  /**
    * Remove all created files from the device
    *
    * @param verbose only print information about progress to stdout if true
    */
  private def cleanup(verbose: Boolean = false): Unit = {
    if (verbose)
      println("Cleaning up created files...")

    try {
      for ((f, _) <- files) {
        if (verbose)
          println(s"Cleanup: Deleting $f")
        new File(s"$mountPoint/$f").delete()
        files -= f
      }
    } catch {
      case _: Exception =>
        println("Cleanup failed! Files need to be deleted manually!")
    }

    if (verbose)
      println("Cleanup complete!")
  }

  /**
    * Delete some of the created files to free up space on a full filesystem
    */
  private def partialCleanup(): Unit = {
    if (files.isEmpty)
      return

    println("Filesystem is full. Deleting some files...")
    for (_ <- 0 to files.keySet.size / 4)
      removeFile(files.keySet.last)
  }

  private def newFile(): Unit = {
    // filename
    val (name, buf) = createFileMetadata()
    println("Creating file %s with length %d" format(name, buf.length))

    try {
      val out = new FileOutputStream(s"$mountPoint/$name")
      out.write(buf)
      out.close()
      files(name) = buf.length
    } catch {
      // file could not be written to device, most likely there is no space left
      // => delete the file without completeness check
      case e: IOException =>
        e.printStackTrace()
        files -= name
        new File(s"$mountPoint/$name").delete()
        partialCleanup()
    }
  }

  private def createFileMetadata(): (UUID, Array[Byte]) = {
    val f = UUID.randomUUID()

    (f, createBuffer())
  }

  /**
    * Creates a byte-array guaranteed not to contain zero-bytes
    *
    * @return a byte array smaller than `maxFileSize`
    */
  private def createBuffer(): Array[Byte] = {
    val length = if (Random.BooleanRandomizer.random()) {
      // file size may not fit exactly into page borders
      getRandInt.random().abs % maxFileSize
    } else {
      // file size is multiple of page size
      (getRandInt.random().abs % maxFilePages) * pageSize
    }

    byteSequence().take(length).toArray
  }

  private def appendToFile(uuid: UUID): Unit = {
    if (files.isEmpty)
      return

    val appendBuffer = createBuffer()
    val newLength = files(uuid) + appendBuffer.length
    println("Appending %d Bytes to %s. New length is %d" format(appendBuffer.length, uuid, newLength))

    try {
      val f = new FileOutputStream(s"$mountPoint/$uuid", true)
      f.write(appendBuffer)
      f.close()

      files(uuid) = newLength
    } catch {
      case e: IOException =>
        e.printStackTrace()
        files -= uuid
        new File(s"$mountPoint/$uuid").delete()
        partialCleanup()
    }
  }

  private def overwriteFile(uuid: UUID): Unit = {
    if (files.isEmpty)
      return

    val newBuffer = createBuffer()
    println("Overwriting file %s. New length is %d" format(uuid, newBuffer.length))

    try {
      val f = new FileOutputStream(s"$mountPoint/$uuid", false)
      f.write(newBuffer)
      f.close()
      files(uuid) = newBuffer.length
    } catch {
      case e: IOException =>
        e.printStackTrace()
        files -= uuid
        new File(s"$mountPoint/$uuid").delete()
        partialCleanup()
    }
  }

  private def removeFile(uuid: UUID, check: Boolean = true): Unit = {
    if (files.isEmpty)
      return

    println(s"Deleting file $uuid")

    if (check)
      checkFile(uuid, files(uuid))

    val f = new File(s"$mountPoint/$uuid")
    f.delete()

    files -= uuid
  }

  /**
    * Check all files for completeness and integrity
    */
  private def checkAllFiles(): Unit = {
    println("Checking all files for completeness... ")

    for ((uuid, size) <- files) {
      checkFile(uuid, size)
    }

    println("Check OK")
  }

  /**
    * Check the given file for completeness and integrity
    *
    * Checks that the size reported by the filesystem and the readable size
    * match the originally written size.
    * Also checks that no "dummy pages" have been inserted to cover up missing
    * pages. The original files have no zero-bytes, dummy pages are
    * filled with zeroes.
    *
    * @param uuid filename to check
    * @param size originally written size
    */
  private def checkFile(uuid: UUID, size: Int): Unit = {
    val f = new File(s"$mountPoint/$uuid")
    val reader = new FileInputStream(f)
    val len = f.length().asInstanceOf[Int]
    val fileContent = new Array[Byte](Math.max(len, size))
    val bytesRead = reader.read(fileContent)

    // check that the file length reported by the OS and the readable bytes
    // matches the written size
    if (f.length() != size || bytesRead != size) {
      throw new InvalidObjectException(s"Actual size of file $uuid is $bytesRead (readable)/$len (reported by filesystem) but expected it to be $size!")
    }

    // no byte should be zero in the written files => if the read bytes contain
    // a zero a page could not be read and we know it was given back as a
    // placeholder
    if (fileContent.contains(0)) {
      throw new InvalidObjectException(s"File $uuid contains zero-bytes!")
    }

    reader.close()
  }

  /**
    * Generates a byte stream that is guaranteed not to contain zero
    *
    * @param start a number to count up from, default: 1
    * @return a byte stream of non-zero bytes
    */
  private def byteSequence(start: Byte = 1): Stream[Byte] = start #:: byteSequence(start match {
    case -1 => 1.asInstanceOf[Byte]
    case _ => (start + 1).asInstanceOf[Byte]
  })
}
