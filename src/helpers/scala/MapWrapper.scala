package helpers.scala

import scala.collection.mutable.Map

/** Wrapper around mutable maps */
sealed abstract class MapWrapperBase[K, T] protected (var map: Map[K, T] = Map[K, T]()) {
  def size: Int = map.size
  def contains(key: K) = map.contains(key)
  def keys: Iterable[K] = map.keys
  def keySet: Set[K] = map.keySet.toSet
  def keySetWrapper: SetWrapper[K] = new SetWrapper[K](map.keySet)
  def apply(key: K): T = map(key)
  def -=(key: K): Unit = map -= key
  def --=(keys: Set[K]): Unit = map --= keys
  def --=(keys: SetWrapper[K]): Unit = map --= keys.set
  def update(key: K, value: T): Unit = map += Tuple2(key, value)
  def +=(keyvalue: (K, T)): Unit = map += keyvalue
  def isEmpty: Boolean = map.isEmpty
  def clear: Unit = map.clear
  def headKey: K = map.head._1

  /* replace the actual map */
  def :=(wrapper: MapWrapperBase[K, T]) {
    map = wrapper.map
  }
}

final class MapWrapper[K, T](__initial_map: Map[K, T] = Map[K, T]()) extends MapWrapperBase[K, T](__initial_map) with DeepCopyable[MapWrapper[K, T]] {
  override def deepCopy(): MapWrapper[K, T] = new MapWrapper[K, T](map.clone())
  override def equals(a: Any): Boolean = a.isInstanceOf[MapWrapper[_, _]] && a.asInstanceOf[MapWrapper[_, _]].map == map
}

final class MapWrapperDeep[K, T <: DeepCopyable[T]](__initial_map: Map[K, T] = Map[K, T]()) extends MapWrapperBase[K, T](__initial_map) with DeepCopyable[MapWrapperDeep[K, T]] {
  override def deepCopy(): MapWrapperDeep[K, T] = {
    val newWrapper = new MapWrapperDeep[K, T]()
    map.foreach { pair =>
      newWrapper += (pair._1, pair._2.deepCopy) // NOTE: does deepCopy of the value
    }
    newWrapper
  }
  override def equals(a: Any): Boolean = a.isInstanceOf[MapWrapperDeep[_, _]] && a.asInstanceOf[MapWrapperDeep[_, _]].map == map
}
