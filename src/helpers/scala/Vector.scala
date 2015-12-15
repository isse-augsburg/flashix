// Flashix: a verified file system for flash memory
// (c) 2015 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package helpers.scala

import scala.reflect.ClassTag

object Vector
{
  def construct[T : ClassTag](elem : T, size : Int) : Vector[T] =
  {
    return Array.fill[T](size){elem}.toVector
  }

  def fill[T : ClassManifest](v : Vector[T], elem : T, offset : Int, length : Int) : Vector[T] =
  {
    v.slice(0, offset) ++ Array.fill[T](length){elem} ++ v.slice(offset + length, v.length)
  }

  def fill[T : ClassManifest](v : Vector[T], elem : T) : Vector[T] =
  {
    construct(elem, v.size)
  }

  def copy[T](v0 : Vector[T], off0 : Int, v1 : Vector[T], off1 : Int, length : Int) : Vector[T] =
  {
    v1.slice(0, off1) ++ v0.slice(off0, off0 + length) ++ v1.slice(off1 + length, v1.length)
  }

  class Randomizer[T] extends helpers.scala.Randomizer[Vector[T]]
  {
    override def random() : Vector[T] = ???
  }
}
