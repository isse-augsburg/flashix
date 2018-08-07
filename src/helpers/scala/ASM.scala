// Flashix: a verified file system for flash memory
// (c) 2015-2018 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package helpers.scala

class InvariantBody(_invariant: => Boolean, _body: => Unit) {
  def invariant = _invariant
  def body = _body
}

trait ASM {
  //
  // Invariants & contracts of the ASM
  //

  private var invariants = scala.collection.mutable.ListBuffer[Unit => Unit]()

  def invariant(message: String, check: => Boolean) = {
    invariants += { Unit => assert(check, message) }
  }

  private def checkInvariants {
    invariants.foreach { check => check() }
  }

  def maintains[Result](body: => Result): Result = {
    checkInvariants
    val result = body
    checkInvariants
    result
  }

  def maintains[Result](body: => Result)(post: => Boolean): Result = {
    checkInvariants
    val result = body
    checkInvariants
    assert(post)
    result
  }

  def establishes[Result](body: => Result): Result = {
    val result = body
    checkInvariants
    result
  }

  def establishes[Result](body: => Result)(post: => Boolean): Result = {
    val result = body
    checkInvariants
    assert(post)
    result
  }

  //
  // Loop invariants
  //

  /** Invariant without captured values */
  def invariant(invariant: => Boolean)(body: => Unit): InvariantBody = {
    new InvariantBody(invariant, body)
  }

  /** Invariant capturing values */
  def invariant[T](values: T)(invariant: T => Boolean)(body: => Unit): InvariantBody = {
    new InvariantBody(invariant(values), body)
  }

  /** Replaces
   *    while (cond) {
   *      body
   *    }
   *  by
   *    loop (cond) {
   *      invariant(values) {
   *        case oldvalues =>
   *          <invariant condition>
   *      } {
   *        body
   *      }
   *    }
   **/
  def loop(condition: => Boolean)(invariantbody: InvariantBody): Unit = {
    while (condition) {
      assert(invariantbody.invariant, "invariant check failed")
      invariantbody.body
    }
    assert(invariantbody.invariant, "invariant check failed")
  }
}
