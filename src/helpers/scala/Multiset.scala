package helpers.scala

final case class Multiset[T](private val map : Map[T, Int] = Map[T, Int]())
{
  def this(x : T) = this(Map(x -> 1))
  def apply(x : T) : Int = map.getOrElse(x, 0)
  def + (x : T) : Multiset[T] = Multiset(map + (x -> (map.getOrElse(x, 0) + 1)))
  def - (x : T) : Multiset[T] =
  {
    assertInvariant()
    if (map.contains(x))
      Multiset(map + (x -> (map(x) - 1)))
    else
      this
  }
  def ∪ (ms : Multiset[T]) : Multiset[T] =
  {
    val unionMap = (map.keySet ++ ms.map.keySet).map(x => (x, apply(x) + ms(x))).toMap
    Multiset(unionMap)
  }
  def -- (ms : Multiset[T]) : Multiset[T] =
  {
    var result = Map[T, Int]()
    map.foreach(pair =>
    {
      val resCount = pair._2 - ms(pair._1)
      if (resCount > 0)
        result += pair._1 -> resCount
    })
    Multiset(result)
  }
  def size : Int = map.foldLeft(0)((value, pair) => value + pair._2)
  def contains(x : T) : Boolean = apply(x) != 0
  def ⊆ (ms : Multiset[T]) : Boolean = map.forall(pair => ms(pair._1) >= pair._2)

  private def assertInvariant() = assert(map.forall(_._2 > 0))
}

object Multiset
{
  implicit class Randomizer[T](t : T) extends helpers.scala.Randomizer[Multiset[T]]
  {
    override def random() : Multiset[T] = ???
  }
}
