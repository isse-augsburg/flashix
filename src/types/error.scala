package types

import helpers.scala._

object error extends helpers.scala.Random.Enumeration {
  type error = Value
  val ESUCCESS, EFAIL, ENOMEM, ENOSPC, EIO, EEXISTS, ENOENT, EISDIR, ENOTDIR, ENOTEMPTY, EBADFD, EACCESS, EINVAL, ECOMMIT = Value

  def uninit = EEXISTS

  implicit object errorRandomizer extends helpers.scala.Randomizer[error.error] {
    override def random(): error.error = error.random()
  }
}
