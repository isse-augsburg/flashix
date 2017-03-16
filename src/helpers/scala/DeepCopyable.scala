package helpers.scala

trait DeepCopyable[T] {
  def deepCopy(): T
}
