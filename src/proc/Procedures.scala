// Flashix: a verified file system for flash memory
// (c) 2015-2017 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package proc

import encoding.node_header._
import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._

trait Procedures {
  def debug(name: String)  (implicit _algebraic_implicit: algebraic.Algebraic)
  def flashix_check_axioms()  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    assert(! isempty(validtrailer), """axiom  ⊦ ¬ isempty(validtrailer) does not hold""")
    assert(is_aligned(EB_PAGE_SIZE, 2 * NODE_HEADER_SIZE), """axiom  ⊦ is-aligned(EB_PAGE_SIZE, 2 * NODE_HEADER_SIZE) does not hold""")
  }
}
