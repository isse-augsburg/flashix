package visualization.models

import visualization._
import visualization.Toolkit._
import integration.Flashix
import types._

object Index extends Tab with Observable[Graph[String]] {
  val empty = Graph.empty[String]

  val model = new GraphModel(empty)
  val view = new GraphView(model)

  this += model

  val page = tab("Index", view)

  def node(r: znode): String = {
    System.identityHashCode(r).toString
  }

  def graph(adr: address, r: znode): Graph[String] = {
    if (r == null) {
      empty
    } else if (r.leaf) {
      Graph[String](Set(node(r)), Set())
    } else {
      val n = node(r)
      val gs = (0 until r.usedsize) map {
        i =>
          val zbr = r.zbranches(i)
          graph(zbr.adr, zbr.child)
      }

      val es = for(i <- 0 until r.usedsize if r.zbranches(i).child != null) yield  {
          val zbr = r.zbranches(i)
          (n, node(zbr.child))
      }

      val g0 = Graph(Set(n), es.toSet)

      gs.foldLeft(g0)(_ ++ _)
    }
  }

  def apply(flashix: Flashix) {
    println("update!")
    update(graph(flashix.btree.ADRT, flashix.btree.RT))
  }
}