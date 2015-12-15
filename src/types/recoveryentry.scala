package types

import helpers.scala._

final case class recoveryentry(var pnum: Int, var sqn: Int) extends DeepCopyable[recoveryentry] {
  override def deepCopy(): recoveryentry = recoveryentry(pnum, sqn)
}

object recoveryentry {
  /**
   * Functions for constructors
   */
  def recovery_entry(pnum: Int, sqn: Int) : recoveryentry = {
    recoveryentry(pnum, sqn)
  }

  def uninit = recovery_entry(0, 0)

  implicit object Randomizer extends helpers.scala.Randomizer[recoveryentry] {
    def random() : recoveryentry = recoveryentry(helpers.scala.Random[Int], helpers.scala.Random[Int])
  }
}
