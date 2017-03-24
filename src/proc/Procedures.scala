// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package proc

import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._

trait Procedures {
  def debug(str: String)  (implicit _algebraic_implicit: algebraic.Algebraic)
  def flashix_check_axioms()  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    
  }
  def isempty(buf: buffer, boolvar: Ref[Boolean])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    isempty_h(buf, 0, buf.length, boolvar)
  }
  def isempty_h(buf: buffer, n0: Int, pageno: Int, boolvar: Ref[Boolean])  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    var n1: Int = pageno
    while (n1 != 0 && buf((n0 + n1) - 1) == empty) {
      n1 = n1 - 1
    }
    boolvar := (n1 == 0)
  }
}
