// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package helpers.scala

object SetExt {
  /** Match the empty set constructor */
  object Ø {
    def apply[T]() : Set[T] = scala.collection.immutable.Set[T]()
    def unapply[T](s : Set[T]) : Boolean = s.isEmpty
  }

  /** Match the insert constructor of sets */
  object ++ {
    def apply[T](s : Set[T], elem : T) : Set[T] = s + elem
    /** @note: May not be used to restrict either argument further, i.e., bound variables
     *         for the element and the remaining set must be unconstrained, see
     *         http://stackoverflow.com/questions/15163904/scala-pattern-matching-with-sets
     *         for more information */
    def unapply[T](s : Set[T]) : Option[(Set[T], T)] = if (s.isEmpty) None else Some((s.tail, s.head))
  }

  implicit class Randomizer[T](t : T) extends helpers.scala.Randomizer[Set[T]] {
    override def random() : Set[T] = ???
  }
}
