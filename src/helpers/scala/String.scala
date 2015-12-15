package helpers.scala

object String
{
  implicit object Randomizer extends Randomizer[String]
  {
    override def random() : String = ???
  }
}
