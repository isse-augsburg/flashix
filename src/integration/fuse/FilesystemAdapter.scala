// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package integration.fuse

import java.nio._
import java.io._
import fuse._
import asm._
import helpers.scala._
import types._
import types.error.error
import debug.Debug
import integration.Flashix

import scala.annotation.tailrec
import scala.language.implicitConversions

object OpenMode {
  // <fcntl.h>
  def isRead(flags: Int) = ((flags % 2) == 0)
  def isWrite(flags: Int) = ((flags & 3) > 0)
  def isCreat(flags: Int) = ((flags & 0x40) != 0)
  def isTrunc(flags: Int) = ((flags & 0x200) != 0)
  def isAppend(flags: Int) = ((flags & 0x400) != 0)

  def toMode(flags: Int) = {
    val r = isRead(flags)
    val w = isWrite(flags)
    if (r && w) file_mode.MODE_RW
    else if (w) file_mode.MODE_W
    else file_mode.MODE_R
  }
}

case class FH(fd: Int, isAppend: Boolean)

object Metadata {
  val DEFAULT_DIR_PERM = 0x1ED // 0755
  val DEFAULT_FILE_PERM = 0x1A4 // 0644
  val DEFAULT_SYMLINK_PERM = 0x1FF // 0777

  /*
  val GID = 0
  val UID = 0
  val UMASK = 0x1FF // 0777
*/

  def now = (System.currentTimeMillis / 1000).toInt

  implicit class MetadataOps(md: metadata)(implicit _algebraic_implicit: algebraic.Algebraic) {
    import _algebraic_implicit._
    def put(getattrSetter: FuseGetattrSetter, inode: Int, nlink: Int, size: Int) {
      val blocks = (size + 512 - 1) / 512 // specifically, number of 512B blocks -- VFS_PAGE_SIZE is wrong here
      getattrSetter.set(inode, md.mode, nlink, md.uid, md.gid, md.mode & FuseFtypeConstants.TYPE_MASK, size, blocks, md.atime, md.mtime, md.ctime)
    }

    def isDir = (md.mode & FuseFtypeConstants.TYPE_MASK) == FuseFtypeConstants.TYPE_DIR
    def isFile = (md.mode & FuseFtypeConstants.TYPE_MASK) == FuseFtypeConstants.TYPE_FILE

    def chmod(_mode: Int) = {
      md.copy(mode = _mode)
    }
    def chown(_uid: Int, _gid: Int) = {
      md.copy(uid = _uid, gid = _gid)
    }
    def utime(_atime: Int, _mtime: Int) = {
      md.copy(atime = _atime, mtime = _mtime)
    }
  }
}

object DirMetadata {
  def apply(mode: Int = Metadata.DEFAULT_DIR_PERM) = {
    val time = Metadata.now
    // TODO: uid/gid
    metadata(mode | FuseFtypeConstants.TYPE_DIR, 0, 0, time, 0, time, 0, time, 0)
  }
}

object FileMetadata {
  def apply(mode: Int = Metadata.DEFAULT_FILE_PERM) = {
    val time = Metadata.now
    // TODO: uid/gid
    metadata(mode | FuseFtypeConstants.TYPE_FILE, 0, 0, time, 0, time, 0, time, 0)
  }
}

object SymlinkMetadata {
  def apply(mode: Int = Metadata.DEFAULT_SYMLINK_PERM) = {
    val time = Metadata.now
    // TODO: uid/gid
    metadata(mode | FuseFtypeConstants.TYPE_SYMLINK, 0, 0, time, 0, time, 0, time, 0)
  }
}

class FilesystemAdapter(flashix: Flashix)(implicit _algebraic_implicit: algebraic.Algebraic, _procedures_implicit: proc.Procedures) extends Filesystem3 {
  import _algebraic_implicit._
  import _procedures_implicit._
  import Metadata._

  import flashix.posix
  import flashix.journal
  import flashix.persistence
  import flashix.persistence_io

  val user: Byte = 0 // TODO: DefaultUser

  def errorToErrno(err: error): Int = err match {
    case error.ESUCCESS =>
      0
    case error.ENOMEM =>
      Errno.ENOMEM
    case error.ENOSPC =>
      Errno.ENOSPC
    case error.EIO =>
      Errno.EIO
    case error.EEXISTS =>
      Errno.EEXIST
    case error.ENOENT =>
      Errno.ENOENT
    case error.EISDIR =>
      Errno.EISDIR
    case error.ENOTDIR =>
      Errno.ENOTDIR
    case error.ENOTEMPTY =>
      Errno.ENOTEMPTY
    case error.EBADFD =>
      Errno.EBADFD
    case error.EACCESS =>
      Errno.EACCES
    case error.EINVAL =>
      Errno.EINVAL
    case error.ECOMMIT =>
      Errno.EUCLEAN
  }

  def throwLog(e: Throwable) = {
    debug("Error in the file system: " + e)
    throw e
  }

  def doGC(reason: String, ERR: Ref[error], free_lebs: Int) = {
    debug(s"flashix: attempting garbage collection with ${free_lebs} LEBs (${reason})")

    journal.aubifs_internal_check_commit(ERR)

    if (ERR.get == error.ESUCCESS) {
      journal.journal_gc()
    } else {
      debug(s"flashix: garbage collection impossible")
    }
  }

  def GCcritical(tryCommit: Boolean, free: Int, log: Int, ERR: Ref[error]): Unit = {
    if (tryCommit && log != 0 && free == 0) {
      val (total_bytes, free_bytes) = flashix.computeStats

      if (free_bytes > LEB_SIZE * (4 + free)) {
        debug(s"flashix: attempting to free ${free_bytes} bytes from the log")
        journal.aubifs_commit(ERR)

        GCloop(tryCommit, ERR)
      }
    } else {
      debug(s"flashix: no blocks available for garbage collection")
    }
  }

  def GCloop(tryCommit: Boolean, ERR: Ref[error]): Unit = {
    val (total, free, log) = flashix.mainAreaLEBs
    val isCritical = free < flashix.percentOf(10, total)

    val N = new Ref[Int](0)
    persistence.apersistence_get_gc_block(N, ERR)

    if (ERR.get == error.ESUCCESS) {
      val lp = persistence.LPT(N.get)

      val isEasy = lp.ref_size < flashix.percentOf(30, LEB_SIZE)
      val eligible = flashix.isBlockEligibleForGC(lp)

      if (!eligible) {
        if (isCritical) GCcritical(tryCommit, free, log, ERR)
      } else if (isEasy || isCritical) {
        doGC(if (isEasy) "easy" else "critical", ERR, free)
        if (ERR.get == error.ESUCCESS)
          GCloop(tryCommit, ERR)
      }
    } else {
      GCcritical(tryCommit, free, log, ERR)
    }
  }

  def checkGC(): Unit = {
    val ERR = new Ref[error](error.ESUCCESS)
    GCloop(true, ERR)
  }

  def isFull: Boolean = {
    val (_, free, _) = flashix.mainAreaLEBs
    free == 0
  }

  def checked[A](operation: Ref[error] => A) = {
    val err = new Ref[error](error.ESUCCESS)

    val res = try {
      operation(err)
    } catch {
      case e: NotImplementedError =>
        e.printStackTrace(System.out)
        throwLog { new FuseException(e).initErrno(Errno.ENOSYS) }
      case e: Throwable =>
        e.printStackTrace(System.out)
        throwLog { new FuseException(e).initErrno(Errno.ENOSYS) }
    }
    if (err.get != error.ESUCCESS)
      throw new FuseException("Unsuccessful operation: " + err.get).initErrno(errorToErrno(err.get))
    res
  }

  def _run(force: Boolean, operation: Ref[error] => Unit): Int = {
    try {
      checkGC()
    } catch {
      case e: Throwable =>
        e.printStackTrace(System.out)
    }

    if (isFull && !force) {
      throw new FuseException().initErrno(Errno.ENOSPC)
    } else {
      checked(operation)
    }

    0
  }

  def run(operation: Ref[error] => Unit) = _run(false, operation)
  def runAlways(operation: Ref[error] => Unit) = _run(true, operation)

  implicit def splitPath(path: String) = {
    path.split(File.separatorChar).toList match {
      case "" :: xs => xs
      case xs => xs
    }
  }

  def getattr(path: String, getattrSetter: FuseGetattrSetter): Int = {
    val md = new Ref[metadata](metadata.uninit)
    val nlink = new Ref[Int](0)
    val size = new Ref[Int](0)
    runAlways(posix.posix_readmeta(path, user, md, nlink, size, _))
    // TODO: inode number
    md.get.put(getattrSetter, 0, nlink.get, size.get)
    0
  }

  def getdir(path0: String, dirFiller: FuseDirFiller): Int = {
    val path: List[String] = path0
    val names = new stringset()

    runAlways {
      posix.posix_readdir(path, user, names, _)
    }

    val md = new Ref[metadata](metadata.uninit)
    val nlink = new Ref[Int](0)
    val size = new Ref[Int](0)

    runAlways {
      posix.posix_readmeta(path, user, md, nlink, size, _)
    }

    dirFiller.add(".", 0, md.get.mode) // TODO: inode number

    if (path.isEmpty) {
      dirFiller.add("..", 0, md.get.mode) // TODO: inode number
    } else {
      runAlways {
        posix.posix_readmeta(path.init, user, md, nlink, size, _)
      }
      dirFiller.add("..", 0, md.get.mode) // TODO: inode number
    }

    names.set.foreach { name =>
      runAlways {
        posix.posix_readmeta(path :+ name, user, md, nlink, size, _)
      }
      dirFiller.add(name, 0, md.get.mode) // TODO: inode number
    }

    0
  }

  def mknod(path: String, mode: Int, rdev: Int): Int = {
    checked {
      _ => rdev == 0 || ???
    }

    run {
      posix.posix_create(path, FileMetadata(mode), user, _)
    }
  }

  def mkdir(path: String, mode: Int): Int = {
    run {
      posix.posix_mkdir(path, DirMetadata(mode), user, _)
    }
  }

  def unlink(path: String): Int = {
    runAlways {
      posix.posix_unlink(path, user, _)
    }
  }

  def rmdir(path: String): Int = {
    runAlways {
      posix.posix_rmdir(path, user, _)
    }
  }

  def symlink(from: String, to: String): Int = {
    run /* */ { posix.posix_create(to, SymlinkMetadata( /* default */ ), user, _) }

    val fd = new Ref[Int](0)
    val n = new Ref[Int](from.length)
    val buf = new buffer(n.get)
    from.getBytes(0, n.get, buf.array, 0)

    runAlways { posix.posix_open(to, file_mode.MODE_W, user, fd, _) }
    run /* */ { posix.posix_write(fd.get, buf, user, n, _) }
    runAlways { posix.posix_close(fd.get, user, _) }

    0
  }

  def readlink(path: String, link: CharBuffer): Int = {
    val fd = new Ref[Int](0)
    val n = new Ref[Int](link.limit)
    val buf = new buffer(n.get)

    runAlways { posix.posix_open(path, file_mode.MODE_R, user, fd, _) }
    run /* */ { posix.posix_read(fd.get, user, buf, n, _) }
    runAlways { posix.posix_close(fd.get, user, _) }

    link.append(new String(buf.array))

    0
  }

  def rename(from: String, to: String): Int = {
    run {
      posix.posix_rename(from, to, user, _)
    }
  }

  def link(from: String, to: String): Int = {
    run {
      posix.posix_link(from, to, user, _)
    }
  }

  def setattr(path0: String, f: metadata => metadata) = {
    val path: List[String] = path0
    val md = new Ref[metadata](metadata.uninit)
    val nlink = new Ref[Int](0)
    val size = new Ref[Int](0)

    runAlways {
      posix.posix_readmeta(path, user, md, nlink, size, _)
    }

    val md0 = f(md.get)

    run {
      posix.posix_writemeta(path, md0, user, _)
    }
  }

  def chmod(path: String, mode: Int): Int = {
    setattr(path, _.chmod(mode))
  }

  def chown(path: String, uid: Int, gid: Int): Int = {
    setattr(path, _.chown(uid, gid))
  }

  def truncate(path: String, size: Long): Int = {
    runAlways {
      posix.posix_truncate(path, size.toInt, user, _)
    }
  }

  def utime(path: String, atime: Int, mtime: Int): Int = {
    setattr(path, _.utime(atime, mtime))
  }

  def statfs(statfsSetter: FuseStatfsSetter): Int = {
    val (total_bytes, free_bytes) = flashix.computeStats

    val bsize = VFS_PAGE_SIZE
    val blocks = total_bytes / bsize
    val free = free_bytes / bsize
    val avail = free_bytes / bsize
    statfsSetter.set(bsize, blocks, free, avail, 0, 0, 255)

    0
  }

  def open(path: String, flags: Int, openSetter: FuseOpenSetter): Int = {
    runAlways {
      _ => (!OpenMode.isCreat(flags) && !OpenMode.isTrunc(flags)) || ???
    }

    val fd = new Ref[Int](0)
    runAlways {
      posix.posix_open(path, OpenMode.toMode(flags), user, fd, _)
    }

    val fh = FH(fd.get, OpenMode.isAppend(flags))
    openSetter.setFh(fh)
    0
  }

  def read(path: String, fh: AnyRef, bbuf: ByteBuffer, offset: Long): Int = {
    val n = new Ref(bbuf.limit)
    val FH(fd, _) = fh.asInstanceOf[FH]
    val buf = new buffer(n.get)

    runAlways {
      posix.posix_seek(fd, seekflag.SEEK_SET, user, new Ref(offset.toInt), _)
    }

    runAlways {
      posix.posix_read(fd, user, buf, n, _)
    }

    bbuf.put(buf.array)
    0
  }

  def write(path: String, fh: AnyRef, isWritepage: Boolean, bbuf: ByteBuffer, offset: Long): Int = {
    val n = new Ref(bbuf.limit)
    val FH(fd, isAppend) = fh.asInstanceOf[FH]
    val buf = new buffer(n.get)
    bbuf.get(buf.array)

    checked {
      posix.posix_seek(fd, if (isAppend) seekflag.SEEK_END else seekflag.SEEK_SET, user, new Ref(if (isAppend) 0 else offset.toInt), _)
    }

    run { posix.posix_write(fd, buf, user, n, _) }
  }

  def flush(path: String, fh: AnyRef): Int = {
    0
  }

  def release(path: String, fh: AnyRef, flags: Int): Int = {
    val fd = fh.asInstanceOf[FH]
    runAlways {
      posix.posix_close(fd.fd, user, _)
    }
  }

  def fsync(path: String, fh: AnyRef, isDatasync: Boolean): Int = 0
}
