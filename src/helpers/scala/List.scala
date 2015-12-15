// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package helpers.scala

object List
{
  implicit class Randomizer[T](t : T) extends helpers.scala.Randomizer[List[T]]
  {
    override def random() : List[T] = ???
  }
}
