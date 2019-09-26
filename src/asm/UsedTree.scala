// Flashix: a verified file system for flash memory
// (c) 2015-2019 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package asm

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import java.util.concurrent.locks._

abstract class UsedTreeInterface extends ASM {
  def get_max_below_threshold(Threshold: Int, Index: Ref[Int], Counter: Ref[Int])
  def get_min(Index: Ref[Int], Counter: Ref[Int])
  def init()
  def insert(Index: Int, Counter: Int)
  def is_empty(IsEmpty: Ref[Boolean])
  def remove(Index: Int, Counter: Int)
}
