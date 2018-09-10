// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

/**
 * Types that need to be implemented by the user Implementation restrictions:
 *   * the types must be immutable in order to prevent introducing sharing among in the generated code
 */
package object sorts {
  type user = Byte
  def uninit_user: user = ???
}