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
