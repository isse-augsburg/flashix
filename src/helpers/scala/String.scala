// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package helpers.scala

object String
{
  implicit object Randomizer extends Randomizer[String]
  {
    override def random() : String = ???
  }
}
