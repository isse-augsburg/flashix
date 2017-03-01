// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package helpers.scala

class Ref[A](var get: A) {
  def :=(x: A) { set(x) }
  def set(x: A) { get = x }
  override def equals(other: Any) = (get == other)
  override def toString = "Ref(" + get.toString + ")"
}

object Ref {
  def apply[A](a: A) = new Ref(a)
  def empty[A] = new Ref[A](null.asInstanceOf[A])
}
