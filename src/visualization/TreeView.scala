package visualization

import scala.swing.Component
import Toolkit._
import java.awt.Dimension
import javax.swing.JPanel
import java.awt.Graphics

case class Node(label: String, sub: List[Node], width: Int, height: Int)

class TreeView extends JPanel with Observer[Tree] {
  def WIDTH = 48
  def HEIGHT = 32
  def HSPACE = 16
  def VSPACE = 16

  def PADDING = 32

  // var tree: Tree = null
  var node: Node = null

  override def preferredSize = dimension

  def dimension = {
    if (node != null) {
      val width = node.width + 2 * PADDING
      val height = node.height + 2 * PADDING
      println("preferred size: " + width + " × " + height)
      new Dimension(width, height)
    } else {
      new Dimension(100, 100)
    }
  }

  def apply(tree: Tree) {
    node = measure(tree)
    revalidate()
    repaint()
  }

  override def paintComponent(g: Graphics) {
    super.paintComponent(g)

    println("painting")

    if (node != null) {
      val x = PADDING
      val y = PADDING
      paint(x, y, node, g)
    }
  }

  def paint(x: Int, y: Int, node: Node, g: Graphics) {
    val nx = x + (node.width - WIDTH) / 2
    val ny = y

    g.drawString(node.label, nx + 4, ny + HEIGHT / 2 + 4)
    g.drawRect(nx, ny, WIDTH, HEIGHT)

    var offset = x
    for (n <- node.sub) {
      val sx = offset
      val sy = y + HEIGHT + VSPACE
      offset += n.width + HSPACE
      g.drawLine(nx + WIDTH / 2, ny + HEIGHT, sx + n.width / 2, sy)
      paint(sx, sy, n, g)
    }
  }

  def measure(tree: Tree): Node = {
    val label = tree.label

    if (tree.isLeaf) {
      Node(label, Nil, WIDTH, HEIGHT)
    } else {
      val vspace = HEIGHT + VSPACE
      val hspace = (tree.sub.length - 1) * HSPACE
      val nodes = tree.sub map measure
      val widths = nodes.map(_.width)
      val heights = nodes.map(_.height)
      Node(label, nodes, widths.sum + hspace, heights.max + vspace)
    }
  }
}
