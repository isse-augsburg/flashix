package visualization.models

import visualization.Tab
import visualization.Observable
import visualization.Tree
import visualization.TreeView
import visualization.Toolkit._
import integration.Flashix
import types._
import scala.swing._

object SimpleIndex extends Tab with Observable[Tree] {
  val view = new TreeView()

  this += view

  val scrollPane = new ScrollPane(view)
  val page = tab("Index", scrollPane)

  def tree(adr: address, r: znode): Tree = {
    if (r == null) {
      Tree("<null>", false)
    } else if (r.leaf) {
      Tree("leaf", r.dirty)
    } else {
      val ts = (0 until r.usedsize) map {
        i =>
          val zbr = r.zbranches(i)
          tree(zbr.adr, zbr.child)
      }
      Tree("node", r.dirty, ts.toList)
    }
  }

  def apply(flashix: Flashix) {
    update(tree(flashix.btree.ADRT, flashix.btree.RT))
  }
}