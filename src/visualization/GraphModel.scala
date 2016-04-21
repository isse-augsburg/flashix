package visualization

import com.mxgraph.view.mxGraph
import com.mxgraph.layout.hierarchical.mxHierarchicalLayout
import com.mxgraph.layout.mxIGraphLayout

class GraphModel[T <: AnyRef](var graph: Graph[T]) extends mxGraph with Observer[Graph[T]] {
  val NODE_WIDTH = 150
  val NODE_HEIGHT = 35

  var nodesMap: Map[T, Object] = Map()
  var edgesMap: Map[(T, T), Object] = Map()

  val layout = new mxHierarchicalLayout(this)

  createGraph()

  def apply(newGraph: Graph[T]) {
    getModel.beginUpdate()
    addGraph(newGraph -- graph)
    removeGraph(graph -- newGraph)
    getModel.endUpdate()

    graph = newGraph
    setLayout()
    repaint()
  }

  def createGraph() {
    addGraph(graph)
    setLayout()
  }

  def addGraph(g: Graph[T]) {
    if (g != null && g.nodes != null && g.edges != null) {
      nodesMap ++= g.nodes.map(createVertex).toMap
      edgesMap ++= g.edges.map(createEdge).toMap
    }
  }

  def removeGraph(g: Graph[T]) {
    val (cells1, cells2) = g.map(nodesMap, edgesMap)
    val cells = cells1 ++ cells2

    this.removeCells(cells.toArray)
    nodesMap --= g.nodes
    edgesMap --= g.edges
  }

  def createVertex(node: T) = {
    (node, this.insertVertex(null, null, node.toString(), 0, 0, NODE_WIDTH, NODE_HEIGHT, "fillColor=yellow"))
  }

  def createEdge(edge: (T, T)) = {
    (edge, this.insertEdge(nodesMap(edge._1), null, "", nodesMap(edge._1), nodesMap(edge._2)))

  }

  private def setLayout() {
    layout.execute(this.getDefaultParent())
  }

}