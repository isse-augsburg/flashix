package helpers.scala

object Store
{
  /** Match the empty store constructor */
  object Ø
  {
    def apply[T0, T1]() : Map[T0, T1] = scala.collection.immutable.Map[T0, T1]()
    def unapply[T0, T1](s : Map[T0, T1]) : Boolean = s.isEmpty
  }

  /** Match the insert/update constructor of stores */
  object ++
  {
    def apply[T0, T1](s : Map[T0, T1], elem : T0, data : T1) : Map[T0, T1] = s + (elem -> data)
    /** @note: May not be used to restrict either argument further, i.e., bound variables
     *         for the element and the remaining set must be unconstrained, see
     *         http://stackoverflow.com/questions/15163904/scala-pattern-matching-with-sets
     *         for more information */
    def unapply[T0, T1](s : Map[T0, T1]) : Option[(Map[T0, T1], (T0, T1))] = if (s.isEmpty) None else Some((s.tail, s.head))
  }
}
