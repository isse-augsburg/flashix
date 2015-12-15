package helpers.scala

object List
{
  implicit class Randomizer[T](t : T) extends helpers.scala.Randomizer[List[T]]
  {
    override def random() : List[T] = ???
  }
}
