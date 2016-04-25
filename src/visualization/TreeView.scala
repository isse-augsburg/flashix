package visualization

import scala.swing.Component
import Toolkit._
import javax.swing.JPanel
import java.awt._

case class Node(label: String, isDirty: Boolean, sub: scala.List[Node], width: Int, height: Int)

class TreeView extends JPanel with Observer[Tree] {
  def WIDTH = 28
  def HEIGHT = 48

  def PADDING = 32

  // var tree: Tree = null
  var node: Node = null

  override def preferredSize = dimension

  def dimension = {
    if (node != null) {
      val width = node.width + 2 * PADDING
      val height = node.height + 2 * PADDING
      new Dimension(width, height)
    } else {
      super.preferredSize
    }
  }

  def apply(tree: Tree) {
    node = measure(tree)
    revalidate()
    repaint()
  }

  override def paintComponent(_g: Graphics) {
    val g = _g.asInstanceOf[Graphics2D]
    super.paintComponent(g)

    g.setRenderingHint(
      RenderingHints.KEY_TEXT_ANTIALIASING,
      RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

    g.setRenderingHint(
      RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON);

    g.setStroke(new BasicStroke(1.5f))

    if (node != null) {
      val x = PADDING
      val y = PADDING + HEIGHT / 2
      paint(x, y, node, g)
    }
  }

  def paint(x: Int, y: Int, node: Node, g: Graphics) {
    val nx = x + node.width / 2
    val ny = y

    var offset = x
    for (n <- node.sub) {
      val sx = offset
      val sy = y + HEIGHT
      offset += n.width

      g.setColor(Color.gray)
      g.drawLine(nx, ny, sx + n.width / 2, sy)
      paint(sx, sy, n, g)
    }

    // g.drawString(node.label, nx + 4, ny + HEIGHT / 2 + 4)
    if (node.label == "<null>") {
      g.setColor(Color.black)
      g.fillOval(nx - 4, ny - 4, 8, 8)
    } else {
      val bg = if (node.isDirty) Color.red else Color.cyan
      val fg = Color.black

      if (node.label == "leaf") {
        g.setColor(bg)
        g.fillOval(nx - 6, ny - 6, 12, 12)
        g.setColor(fg)
        g.drawOval(nx - 6, ny - 6, 12, 12)
      } else if(node.label == "clean") {
        val xs = Array(nx, nx - 12, nx + 12)
        val ys = Array(ny, ny + 16, ny + 16)
        g.setColor(bg)
        g.fillPolygon(xs, ys, 3)
        g.setColor(fg)
        g.drawPolygon(xs, ys, 3)
      } else if (node.label == "node") {
        g.setColor(bg)
        g.fillRect(nx - 12, ny, 24, 16)
        g.setColor(fg)
        g.drawRect(nx - 12, ny, 24, 16)
      }
    }
  }

  def measure(tree: Tree): Node = {
    val label = tree.label
    val dirty = tree.isDirty
    if (tree.isLeaf) {
      Node(label, dirty, Nil, WIDTH, HEIGHT)
    } else if (!dirty) {
      Node("clean", dirty, Nil, WIDTH, HEIGHT)
    } else {
      val nodes = tree.sub map measure
      val widths = nodes.map(_.width)
      val heights = nodes.map(_.height)
      Node(label, dirty, nodes, widths.sum, heights.max)
    }
  }
}
