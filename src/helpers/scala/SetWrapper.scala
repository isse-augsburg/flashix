package helpers.scala

import scala.collection.mutable.Set
 
/** Wrapper around mutable sets */
class SetWrapper[T] (var set: Set[T] = Set[T]()) extends  {
  def this(elem: T) = this(Set(elem))
  def this(set: scala.collection.Set[T]) = this(Set(set.toSeq:_*))

  def size : Int = set.size
  def contains(key : T) = set.contains(key)
  def -= (key : T) : Unit = set -= key
  def --= (keys : SetWrapper[T]) : Unit = set --= keys.set
  def += (key : T) : Unit = set += key
  def + (key: T): SetWrapper[T] = new SetWrapper[T](set + key)
  def - (key: T): SetWrapper[T] = new SetWrapper[T](set - key)
  def subsetOf(other: SetWrapper[T]): Boolean = set.subsetOf(other.set)
  def union(other: SetWrapper[T]): SetWrapper[T] = new SetWrapper[T](set.union(other.set))
  def intersect(other: SetWrapper[T]): SetWrapper[T] = new SetWrapper[T](set.intersect(other.set))
  def -- (other: SetWrapper[T]): SetWrapper[T] = new SetWrapper[T](set -- other.set)
  def isEmpty: Boolean = set.isEmpty
  def clear: Unit = set.clear

  /* replace the actual map */
  def := (wrapper: SetWrapper[T]) {
    set = wrapper.set
  }

  def deepCopy: SetWrapper[T] = new SetWrapper(set.clone())
}
