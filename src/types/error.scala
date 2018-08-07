// Flashix: a verified file system for flash memory
// (c) 2015-2018 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._

object error extends helpers.scala.Random.Enumeration {
  type error = Value
  val ESUCCESS, EFAIL, ENOMEM, ENOSPC, EIO, EEXISTS, ENOENT, EISDIR, ENOTDIR, ENOTEMPTY, EBADFD, EACCESS, EINVAL, ECOMMIT, EROFS, EUCLEAN = Value

  def uninit = EFAIL

  implicit object errorRandomizer extends helpers.scala.Randomizer[error.error] {
    override def random(): error.error = error.random()
  }
}
