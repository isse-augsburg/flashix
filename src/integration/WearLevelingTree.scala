package integration

import helpers.scala.Ref
import scala.collection.mutable.TreeSet

trait WearLevelingTree {
  var tree: TreeSet[(Int, Int)] = TreeSet[(Int, Int)]() // tuple of (erase counter, peb number)

  def get_max_below_threshold(Threshold: Int, Index: Ref[Int], Counter: Ref[Int]) {
    val (counter, index) = tree.filter { _._1 < Threshold }.max
    Index := index
    Counter := counter
  }
  def get_min(Index: Ref[Int], Counter: Ref[Int]) {
    val (counter, index) = tree.min
    Index := index
    Counter := counter
  }
  def init() {
    tree = TreeSet[(Int, Int)]()
  }
  def insert(Index: Int, Counter: Int) {
    tree += ((Counter, Index))
  }
  def is_empty(IsEmpty: Ref[Boolean]) {
    IsEmpty := tree.isEmpty
  }
  def remove(Index: Int, Counter: Int) {
    tree -= ((Counter, Index))
  }
}
