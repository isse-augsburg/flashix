package helpers.scala

trait Randomizer[T]
{
  def random() : T
}

object Random
{
  val generator = new scala.util.Random()
  def apply[T](implicit R : Randomizer[T]) = R.random()
  def apply[T] = ???

  /** Randomizer for Byte */
  implicit object ByteRandomizer extends helpers.scala.Randomizer[Byte]
  {
	override def random() : Byte =
	{
	  val byteArray = new Array[Byte](1)
	  generator.nextBytes(byteArray)
	  byteArray.head
	}
  }

  /** Randomizer for Int */
  implicit object IntRandomizer extends helpers.scala.Randomizer[Int]
  {
	override def random() : Int = generator.nextInt
  }

  /** Helper class to implement a randomizer for Enumerations */
  abstract class Enumeration extends scala.Enumeration
  {
    def random() : Value =
    {
      val sequence = values.toSeq
      if (sequence.size == 0)
        ???
      val index = generator.nextInt(sequence.size - 1)
      sequence(index)
    }
  }
}
