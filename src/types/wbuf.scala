// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package types

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

final case class wbuf(var content: buffer, var offset: Int, var nbytes: Int) extends DeepCopyable[wbuf] {
  override def deepCopy(): wbuf = wbuf(content.deepCopy, offset, nbytes)

  def := (other: wbuf) {
    content = other.content
    offset = other.offset
    nbytes = other.nbytes
  }
}

object wbuf {
  /**
   * Functions for constructors
   */
  def mkwbuf(content: buffer, offset: Int, nbytes: Int): wbuf = {
    wbuf(content, offset, nbytes)
  }

  def uninit = mkwbuf(new buffer(), 0, 0)

  implicit object Randomizer extends helpers.scala.Randomizer[wbuf] {
    override def random(): wbuf = wbuf(helpers.scala.Random[buffer], helpers.scala.Random[Int], helpers.scala.Random[Int])
  }
}
