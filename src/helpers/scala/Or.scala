// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package helpers.scala

import scala.util._

object Or {
  def apply(fun0: => Unit): Unit = fun0
  def apply(fun0: => Unit, fun1: => Unit): Unit = {
    val option = Random.generator.nextInt(1)
    if (option == 0) fun0
    else fun1
  }
}
