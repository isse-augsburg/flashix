// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package helpers.scala

import scala.reflect.ClassTag

/**
 * Wrapper around mutable arrays
 *  @note We use the wrapper to be able to exchange the array if changing the
 *        actual object is not possible (e.g. because the size could/would change)
 */
sealed abstract class ArrayWrapperBase[T] protected (var array: Array[T]) {
  final def length: Int = array.length
  final def apply(idx: Int): T = array(idx)
  final def update(idx: Int, value: T): Unit = array(idx) = value

  def :=(wrapper: ArrayWrapperBase[T]) {
    array = wrapper.array
  }

}

/** Wrapper around mutable arrays, if T does not contain anything mutable */
final class ArrayWrapper[T : ClassTag](_array: Array[T]) extends ArrayWrapperBase[T](_array) with DeepCopyable[ArrayWrapper[T]] {

  def this(length: Int) = this(new Array[T](length))
  def this() = this(0)

  def allocate(length: Int, elem: T) = {
    array = new Array[T](length)
    fill(elem)
  }

  def fill(elem: T): ArrayWrapper[T] = {
    var i = 0
    while (i < array.size) {
      array(i) = elem
      i = i + 1
    }
    this // NOTE: return this to be able to default-init in every expression
  }

  def fill(elem: T, idx: Int, size: Int): Unit = {
    var i = 0
    while (i < size) {
      array(idx + i) = elem
      i = i + 1
    }
  }

  def slice(from: Int, until: Int) = new ArrayWrapper[T](_array.slice(from, until))

  def copy(src: ArrayWrapper[T], srcPos: Int, destPos: Int, length: Int): Unit = {
    System.arraycopy(src.array, srcPos, array, destPos, length)
  }

  override def deepCopy(): ArrayWrapper[T] = {
    new ArrayWrapper(array.clone())
  }

  override def equals(x: Any) = x.isInstanceOf[ArrayWrapper[T]] && x.asInstanceOf[ArrayWrapper[T]].array.deep == this.array.deep
}

/** wrapper around mutable arrays, if T contains something mutable */
final class ArrayWrapperDeep[T <: DeepCopyable[T] : ClassTag](_array: Array[T]) extends ArrayWrapperBase[T](_array) with DeepCopyable[ArrayWrapperDeep[T]] {

  def this(length: Int) = this(new Array[T](length))
  def this() = this(0)

  def allocate(length: Int, elem: T) = {
    array = new Array[T](length)
    fill(elem)
  }

  /** @note we have to perform a deep copy on every element individually, otherwise
   *        the array elements itself would share */
  def fill(elem: T): ArrayWrapperDeep[T] = {
    var i = 0
    while (i < array.size) {
      array(i) = elem.deepCopy
      i = i + 1
    }
    this // NOTE: return this to be able to default-init in every expression
  }

  def slice(from: Int, until: Int) = {
    val newArray = new Array[T](until - from)
    var index = from
    while (index < until) {
      newArray(index - from) = _array(index).deepCopy
      index += 1
    }
    new ArrayWrapperDeep[T](newArray)
  }

  def copy(src: ArrayWrapperDeep[T], srcPos: Int, destPos : Int, length: Int) {
    var copied = 0
    while (copied < length) {
      _array(destPos + copied) = src(srcPos + copied).deepCopy
      copied += 1
    }
  }

  override def deepCopy(): ArrayWrapperDeep[T] = {
    val newArray = new Array[T](array.length)
    for (i <- 0 until array.length)
      newArray(i) = array(i).deepCopy
    new ArrayWrapperDeep(newArray)
  }

  override def equals(x: Any) = x.isInstanceOf[ArrayWrapperDeep[T]] && x.asInstanceOf[ArrayWrapperDeep[T]].array.deep == this.array.deep
}
