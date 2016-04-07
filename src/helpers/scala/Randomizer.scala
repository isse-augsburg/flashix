// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package helpers.scala

trait Randomizer[T] {
  def random(): T
}

object Random {
  val generator = new scala.util.Random()
  def apply[T](implicit R: Randomizer[T]) = R.random()

  implicit object ByteRandomizer extends helpers.scala.Randomizer[Byte] {
    override def random(): Byte = {
      val byteArray = new Array[Byte](1)
      generator.nextBytes(byteArray)
      byteArray.head
    }
  }

  implicit object IntRandomizer extends helpers.scala.Randomizer[Int] {
    override def random(): Int = generator.nextInt
  }

  implicit object BooleanRandomizer extends helpers.scala.Randomizer[Boolean] {
    override def random(): Boolean = generator.nextBoolean()
  }

  // TODO
  implicit object StringRandomizer extends Randomizer[String] {
    override def random(): String = ??? // generator.nextString(10)
  }

  // TODO
  class ListRandomizer[T](implicit tRandomizer: Randomizer[T]) extends helpers.scala.Randomizer[List[T]] {
    override def random(): List[T] = ???
  }

  // TODO
  class ListWrapperRandomizer[T](implicit tRandomizer: Randomizer[T]) extends helpers.scala.Randomizer[ListWrapper[T]] {
    override def random(): ListWrapper[T] = ???
  }

  // TODO
  class ListWrapperDeepRandomizer[T <: DeepCopyable[T]](implicit tRandomizer: Randomizer[T]) extends helpers.scala.Randomizer[ListWrapperDeep[T]] {
    override def random(): ListWrapperDeep[T] = ???
  }

  // TODO
  class SetRandomizer[T](implicit tRandomizer: Randomizer[T]) extends helpers.scala.Randomizer[Set[T]] {
    override def random(): Set[T] = ???
  }

  class SetWrapperRandomizer[T](implicit tRandomizer: Randomizer[T]) extends helpers.scala.Randomizer[SetWrapper[T]] {
    override def random(): SetWrapper[T] = ???
  }

  // TODO
  class MultisetRandomizer[T](implicit tRandomizer: Randomizer[T]) extends helpers.scala.Randomizer[Multiset[T]] {
    override def random(): Multiset[T] = ???
  }

  // TODO
  class VectorRandomizer[T](implicit tRandomizer: Randomizer[T]) extends helpers.scala.Randomizer[Vector[T]] {
    override def random(): Vector[T] = ???
  }

  // TODO
  class ArrayWrapperRandomizer[T](implicit tRandomizer: Randomizer[T]) extends helpers.scala.Randomizer[ArrayWrapper[T]] {
    override def random(): ArrayWrapper[T] = ???
  }

  // TODO
  class ArrayWrapperDeepRandomizer[T <: DeepCopyable[T]](implicit tRandomizer: Randomizer[T]) extends helpers.scala.Randomizer[ArrayWrapperDeep[T]] {
    override def random(): ArrayWrapperDeep[T] = ???
  }

  // TODO
  class MapRandomizer[K, T](implicit kRandomizer: Randomizer[K], tRandomizer: Randomizer[T]) extends helpers.scala.Randomizer[Map[K, T]] {
    override def random(): Map[K, T] = ???
  }

  // TODO
  class MapWrapperRandomizer[K, T](implicit kRandomizer: Randomizer[K], tRandomizer: Randomizer[T]) extends helpers.scala.Randomizer[MapWrapper[K, T]] {
    override def random(): MapWrapper[K, T] = ???
  }

  // TODO
  class MapWrapperDeepRandomizer[K, T <: DeepCopyable[T]](implicit kRandomizer: Randomizer[K], tRandomizer: Randomizer[T]) extends helpers.scala.Randomizer[MapWrapperDeep[K, T]] {
    override def random(): MapWrapperDeep[K, T] = ???
  }

  /** Helper class to implement a randomizer for Enumerations */
  abstract class Enumeration extends scala.Enumeration {
    def random(): Value = {
      val sequence = values.toSeq
      if (sequence.size == 0)
        ???
      val index = generator.nextInt(sequence.size - 1)
      sequence(index)
    }
  }
}
