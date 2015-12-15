package helpers.scala

import scala.reflect.ClassTag
import scala.collection.mutable.ListBuffer

/** 
 * Wrapper around mutable lists
 */
sealed abstract class ListWrapperBase[T] protected (var list: ListBuffer[T] = new ListBuffer[T]()) {
  def isEmpty: Boolean = list.isEmpty
  def length: Int = list.length
  def head: T = list.head
  def last: T = list.last
  def removeHead: Unit = list.trimStart(1)
  def removeLast: Unit = list.trimEnd(1)
  def contains(elem: T): Boolean = list.contains(elem)
  def clear: Unit = list.clear
  def apply(n: Int) = list(n)
  def hasDuplicates = list.distinct.size == list.size

  /* replace the actual map */
  def := (wrapper: ListWrapperBase[T]) {
    list = wrapper.list
  }

  protected def appendNoCopy(elem: T) = list += elem
  protected def prependNoCopy(elem: T) = list.+=:(elem)
}

final class ListWrapper[T](__initial_list: ListBuffer[T] = new ListBuffer[T]()) extends ListWrapperBase[T](__initial_list) with DeepCopyable[ListWrapper[T]] {
  def this(elems: T*) = this(ListBuffer[T](elems:_*))

  def += (elem: T): Unit = appendNoCopy(elem)
  def ++= (wrapper: ListWrapper[T]) = list ++= wrapper.list
  def +=: (elem: T): Unit = prependNoCopy(elem)
  def distinct = new ListWrapper[T](list.distinct)

  override def deepCopy(): ListWrapper[T] = new ListWrapper(list.clone())
  override def equals(x: Any) = x.isInstanceOf[ListWrapper[T]] && x.asInstanceOf[ListWrapper[T]].list == this.list
}

final class ListWrapperDeep[T <: DeepCopyable[T]](__initial_list: ListBuffer[T] = new ListBuffer[T]()) extends ListWrapperBase[T] with DeepCopyable[ListWrapperDeep[T]]{
  def this(elems: T*) = this(ListBuffer[T](elems:_*))

  def += (elem: T): Unit = appendNoCopy(elem.deepCopy)
  def ++= (wrapper: ListWrapperDeep[T]) = wrapper.list.foreach{ this += _.deepCopy }
  def +=: (elem: T): Unit = prependNoCopy(elem.deepCopy)
  def distinct = new ListWrapper[T](list.distinct.map{_.deepCopy})

  override def deepCopy(): ListWrapperDeep[T] = {
    val newWrapper = new ListWrapperDeep[T]()
    list.foreach{elem =>
      newWrapper.appendNoCopy(elem.deepCopy)
    }
    newWrapper
  }
  override def equals(x: Any) = x.isInstanceOf[ListWrapperDeep[T]] && x.asInstanceOf[ListWrapperDeep[T]].list == this.list
}

/**
 * Immutable functions on lists
 */
object ListWrapper {
  def tail[T](wrapper: ListWrapper[T]): ListWrapper[T] = {
    // NOTE: This implementation is only correct if T does not contain any mutable parts
    val newList = wrapper.list.clone
    newList.trimStart(1)
    new ListWrapper[T](newList)
  }

  def prepend[T](elem: T, wrapper: ListWrapper[T]): ListWrapper[T] = {
    // NOTE: This implementation is only correct if T does not contain any mutable parts
    val copy = wrapper.deepCopy()
    copy.+=:(elem)
    copy
  }

  def prepend[T <: DeepCopyable[T]](elem: T, wrapper: ListWrapperDeep[T]): ListWrapperDeep[T] = {
    val copy = wrapper.deepCopy()
    copy.+=:(elem)
    copy
  }

  def append[T](wrapper: ListWrapper[T], elem: T): ListWrapper[T] = {
    // NOTE: This implementation is only correct if T does not contain any mutable parts
    val copy = wrapper.deepCopy()
    copy += elem
    copy
  }

  def firstn[T](n: Int, wrapper: ListWrapper[T]): ListWrapper[T] = {
    new ListWrapper[T](wrapper.list.take(n))
  }

  def concatenate[T](list1: ListWrapper[T], list2: ListWrapper[T]): ListWrapper[T] = {
    val result = list1.deepCopy
    list2.list.foreach{ result += _ }
    result
  }

  def concatenate[T <: DeepCopyable[T]](list1: ListWrapperDeep[T], list2: ListWrapperDeep[T]): ListWrapperDeep[T] = {
    val result = list1.deepCopy
    list2.list.foreach{ result += _.deepCopy }
    result
  }
}
