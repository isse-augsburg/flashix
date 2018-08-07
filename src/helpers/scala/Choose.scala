// Flashix: a verified file system for flash memory
// (c) 2015-2018 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package helpers.scala

object ChooseIndex
{
  def apply[T](vector : Vector[T], f : Int => Unit, ifnone : => Unit) : Unit =
  {
    if (vector.size == 0)
      ifnone
    else
      f(0)
  }
  def apply[T](vector : Vector[T], f : Int => Unit) : Unit = apply(vector, f, {throw ChooseFailure()})

  def apply[T](vector : Vector[T], p : Int => Boolean, f : Int => Unit, ifnone : => Unit) : Unit =
  {
    ChooseHelper.getFirstIndex(vector, p) match
    {
      case Some(index) => f(index)
      case None => ifnone
    }
  }
  def apply[T](vector : Vector[T], p : Int => Boolean, f : Int => Unit) : Unit = apply(vector, p, f, {throw ChooseFailure()})

  def apply[T](array : ArrayWrapper[T], f : Int => Unit, ifnone : => Unit) : Unit =
  {
    if (array.length == 0)
      ifnone
    else
      f(0)
  }
  def apply[T](array : ArrayWrapper[T], f : Int => Unit) : Unit = apply(array, f, {throw ChooseFailure()})

  def apply[T](array : ArrayWrapper[T], p : Int => Boolean, f : Int => Unit, ifnone : => Unit) : Unit =
  {
    ChooseHelper.getFirstIndex(array.array, p) match
    {
      case Some(index) => f(index)
      case None => ifnone
    }
  }

  def apply[T](array : ArrayWrapper[T], p : Int => Boolean, f : Int => Unit) : Unit = apply(array, p, f, {throw ChooseFailure()})

  def apply[T <: DeepCopyable[T]](array : ArrayWrapperDeep[T], f : Int => Unit, ifnone : => Unit) : Unit =
  {
    if (array.length == 0)
      ifnone
    else
      f(0)
  }
  def apply[T <: DeepCopyable[T]](array : ArrayWrapperDeep[T], f : Int => Unit) : Unit = apply(array, f, {throw ChooseFailure()})

  def apply[T <: DeepCopyable[T]](array : ArrayWrapperDeep[T], p : Int => Boolean, f : Int => Unit, ifnone : => Unit) : Unit =
  {
    ChooseHelper.getFirstIndex(array.array, p) match
    {
      case Some(index) => f(index)
      case None => ifnone
    }
  }

  def apply[T <: DeepCopyable[T]](array : ArrayWrapperDeep[T], p : Int => Boolean, f : Int => Unit) : Unit = apply(array, p, f, {throw ChooseFailure()})
}

object ChooseIn
{
  def apply[T](candidates : Iterable[T], f : T => Unit, ifnone : => Unit) : Unit =
  {
    if (candidates.isEmpty)
      ifnone
    else
      f(candidates.head)
  }
  def apply[T](candidates : Iterable[T], f : T => Unit) : Unit = apply(candidates, f, {throw ChooseFailure()})

  def apply[T](candidates : Iterable[T], p : T => Boolean, f : T => Unit, ifnone : => Unit) : Unit =
  {
    ChooseHelper.getFirstElem(candidates, p) match
    {
      case Some(elem) => f(elem)
      case None => ifnone
    }
  }
  def apply[T](candidates : Iterable[T], p : T => Boolean, f : T => Unit) : Unit = apply(candidates, p, f, {throw ChooseFailure()})
}

object ChooseNotin
{
  def apply[T](excluded : Seq[T], f : T => Unit, ifnone : => Unit)(implicit num : Numeric[T]) : Unit =
  {
    if (excluded.isEmpty)
      f(num.zero)
    else
    {
      val max = excluded.max
      val next = num.plus(max, num.one)
      if (! excluded.contains(next)) // Protect against overflows
        f(next)
      else
        ifnone // TODO: the semantics here are different since we do not know whether we have exhausted all elements
    }
  }
  def apply[T](excluded : Seq[T], f : T => Unit)(implicit num : Numeric[T]) : Unit = apply(excluded, f, {throw ChooseFailure()})

  def apply[T](excluded : Seq[T], p : T => Boolean, f : T => Unit, ifnone : => Unit)(implicit num : Numeric[T]) : Unit =
  {
    val max = if (excluded.isEmpty)
                num.zero
              else
                excluded.max
    var next = max
    do // TODO: could be non-terminating
    {
      next = num.plus(next, num.one)
    }
    while (! p(next) || excluded.contains(next)) // Protect against overflows
    f(next)
  }
  def apply[T](excluded : Seq[T], p : T => Boolean, f : T => Unit)(implicit num : Numeric[T]) : Unit = apply(excluded, p, f, {throw ChooseFailure()})
}

object ChooseRandom
{
  def _1[T](p : T => Boolean, f : T => Unit, ifnone : => Unit = {throw ChooseFailure()})(implicit R : Randomizer[T]) : Unit =
  {
    var candidate = R.random()
    while (! p(candidate))
    {
      candidate = R.random()
    }
    f(candidate)
  }

  def _2[T0, T1](p : (T0, T1) => Boolean, f : (T0, T1) => Unit, ifnone : => Unit = {throw ChooseFailure()})(implicit R0 : Randomizer[T0], R1 : Randomizer[T1]) : Unit =
  {
    var candidate0 = R0.random()
    var candidate1 = R1.random()
    while (! p(candidate0, candidate1))
    {
      candidate0 = R0.random()
      candidate1 = R1.random()
    }
    f(candidate0, candidate1)
  }
}

object ChooseHelper
{
  def getFirstIndex[T](candidates : Seq[T], p : Int => Boolean) : Option[Int] =
  {
    var cur = 0;
    while (cur != candidates.length)
    {
      if (p(cur)) return Some(cur)
      cur = cur + 1
    }
    return None
  }

  def getFirstElem[T](candidates : Iterable[T], p : T => Boolean) : Option[T] =
  {
    val iterator = candidates.iterator
    while (iterator.hasNext)
    {
      val next = iterator.next()
      if (p(next))
        return Some(next)
    }
    return None
  }
}
