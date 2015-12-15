package types

import helpers.scala._

final case class wbuf(var content: buffer, var offset: Int, var nbytes: Int) extends DeepCopyable[wbuf] {
  override def deepCopy(): wbuf = wbuf(content.deepCopy, offset, nbytes)
}

object wbuf {
  /**
   * Functions for constructors
   */
  def mkwbuf(content: buffer, offset: Int, nbytes: Int) : wbuf = {
    wbuf(content, offset, nbytes)
  }

  def uninit = mkwbuf(new buffer(), 0, 0)

  implicit object Randomizer extends helpers.scala.Randomizer[wbuf] {
    def random() : wbuf = wbuf(helpers.scala.Random[buffer], helpers.scala.Random[Int], helpers.scala.Random[Int])
  }
}
