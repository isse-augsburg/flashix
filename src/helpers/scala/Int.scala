// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package helpers.scala

object Int
{
  object plus1
  {
    def apply(n : Int) = n + 1
    def unapply(n : Int) : Option[Int] = {
      assert(n >= 0)
      if (n == 0)
        None
      else
        Some(n - 1)
    }
  }
}
