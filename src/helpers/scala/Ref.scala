package helpers.scala

class Ref[A](var get : A) {
  def := (x : A) { set(x) }
  def set(x : A) { get = x }
  override def equals(other : Any) = (get == other) 
}

object Ref {
  def empty[A] = new Ref[A](null.asInstanceOf[A])
}
