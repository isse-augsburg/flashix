package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

object error extends helpers.scala.Random.Enumeration {
  type error = Value
  val ESUCCESS, EFAIL, ENOMEM, ENOSPC, EIO, EEXISTS, ENOENT, EISDIR, ENOTDIR, ENOTEMPTY, EBADFD, EACCESS, EINVAL, ECOMMIT, EROFS, EUCLEAN = Value

  def uninit = EFAIL

  implicit object errorRandomizer extends helpers.scala.Randomizer[error.error] {
    override def random(): error.error = error.random()
  }
}
