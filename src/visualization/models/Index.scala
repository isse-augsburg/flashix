package visualization.models

import visualization._
import visualization.Toolkit._
import integration.Flashix
import types._

case class Node(key: Int, label: String, color: String, shape: String, width: Int, height: Int) {
  override def toString = label
}

object FancyGraphModel {
  def empty = Graph.empty[Node]
  
  def node(r: znode): Node = {
    val key = System.identityHashCode(r)

    val color = if(r.dirty) "#ff0000" else "#00ccff"

    if(r.leaf) {
      Node(key, "", color, "ellipse", 24, 24)
    } else {
      Node(key, "", color, "rectangle", 32, 16)
    }
  }
}

class FancyGraphModel() extends GraphModel(FancyGraphModel.empty) {
  override def createVertex(node: Node) = {
    (node, this.insertVertex(null, null, node.label, 0, 0, node.width, node.height, "verticalAlign=middle;fontSize=18;fontColor=#000000;shape=" + node.shape + ";fillColor=" + node.color + ";strokeColor=#000000"))
  }

  override def createEdge(edge: (Node, Node)) = {
    val (src, dst) = edge
    (edge, this.insertEdge(nodesMap(src), null, "", nodesMap(src), nodesMap(dst), "strokeColor=#000000"))
  }
}

object Index extends Tab with Observable[Graph[Node]] {
  import FancyGraphModel._
  
  val model = new FancyGraphModel()
  val view = new GraphView(model)

  this += model

  val page = tab("Index", view)

  def graph(adr: address, r: znode): Graph[Node] = {
    if (r == null) {
      empty
    } else if (r.leaf) {
      Graph[Node](Set(node(r)), Set())
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
    update(graph(flashix.btree.ADRT, flashix.btree.RT))
  }
}