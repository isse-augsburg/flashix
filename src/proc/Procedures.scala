// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package proc

import encoding.node_header._
import helpers.scala._
import helpers.scala.Encoding._
import helpers.scala.Random._
import types._

trait Procedures {
  def debug(name: String)  (implicit _algebraic_implicit: algebraic.Algebraic)
  def flashix_check_axioms()  (implicit _algebraic_implicit: algebraic.Algebraic): Unit = {
    import _algebraic_implicit._
    assert(to_vtbl(new volumes()).isEmpty, """axiom  ⊦ to-vtbl(∅) = ∅ does not hold""")
    assert((WL_THRESHOLD == 0) == false, """axiom  ⊦ WL_THRESHOLD = 0 ↔ false does not hold""")
    assert(ROOT_INO != 0, """axiom  ⊦ ROOT_INO ≠ 0 does not hold""")
    assert(VFS_PAGE_SIZE != 0, """axiom  ⊦ VFS_PAGE_SIZE ≠ 0 does not hold""")
    assert(validtrailer.length == NODE_HEADER_SIZE, """axiom  ⊦ # validtrailer = NODE_HEADER_SIZE does not hold""")
    assert(! isempty(validtrailer), """axiom  ⊦ ¬ isempty(validtrailer) does not hold""")
    assert(is_aligned(EB_PAGE_SIZE, 2 * NODE_HEADER_SIZE), """axiom  ⊦ is-aligned(EB_PAGE_SIZE, 2 * NODE_HEADER_SIZE) does not hold""")
    assert(keys(new nat_set()).isEmpty, """axiom  ⊦ ∅.keys = ∅ does not hold""")
    assert(EB_PAGE_SIZE != 0, """axiom  ⊦ EB_PAGE_SIZE ≠ 0 does not hold""")
    assert(PAGES_PER_LEB != 0, """axiom  ⊦ PAGES_PER_LEB ≠ 0 does not hold""")
    assert(EB_PAGE_SIZE != 0, """axiom  ⊦ EB_PAGE_SIZE ≠ 0 does not hold""")
    assert(MIN_SIZE != 0, """axiom  ⊦ MIN_SIZE ≠ 0 does not hold""")
    assert(flashsize(new group_node_list()) == 0, """axiom  ⊦ [].flashsize = 0 does not hold""")
    assert(flashsize(new group_node_list()) == 0, """axiom  ⊦ [].flashsize = 0 does not hold""")
  }
}
