package visualization.models

import java.awt.BasicStroke
import java.awt.Color
import java.awt.Graphics
import java.awt.RenderingHints

import scala.swing.Dimension
import scala.swing.Graphics2D
import scala.swing.ScrollPane

import integration.Flashix
import javax.swing.JPanel
import types.address
import types.znode
import visualization._
import visualization.Toolkit._

object SimpleIndex extends Tab {
  sealed trait Shape
  case object Bullet extends Shape
  case object Circle extends Shape
  case object Triangle extends Shape
  case object Rectangle extends Shape

  def WIDTH = 28
  def HEIGHT = 48
  def PADDING = 32

  case class Node(shape: Shape, color: Color, sub: List[Node] = Nil, width: Int = WIDTH, height: Int = HEIGHT)

  var node: Node = null

  object view extends JPanel {
    // var tree: Tree = null

    override def getPreferredSize = dimension

    def dimension = {
      if (node != null) {
        val width = node.width + 2 * PADDING
        val height = node.height + 2 * PADDING
        new Dimension(width, height)
      } else {
        super.getPreferredSize
      }
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

      val bg = node.color
      val fg = Color.black

      node.shape match {
        case Bullet =>
          g.setColor(Color.black)
          g.fillOval(nx - 4, ny - 4, 8, 8)

        case Circle =>
          g.setColor(bg)
          g.fillOval(nx - 6, ny - 6, 12, 12)
          g.setColor(fg)
          g.drawOval(nx - 6, ny - 6, 12, 12)

        case Triangle =>
          val xs = Array(nx, nx - 12, nx + 12)
          val ys = Array(ny, ny + 16, ny + 16)
          g.setColor(bg)
          g.fillPolygon(xs, ys, 3)
          g.setColor(fg)
          g.drawPolygon(xs, ys, 3)

        case Rectangle =>
          g.setColor(bg)
          g.fillRect(nx - 12, ny, 24, 16)
          g.setColor(fg)
          g.drawRect(nx - 12, ny, 24, 16)
      }
    }

    def measure(adr: address, r: znode): Node = {

      if (r == null) {
        Node(Bullet, Color.black)
      } else {
        val fg = if (r.dirty) Color.red else Color.cyan
        if (r.leaf) {
          Node(Circle, fg)
        } else if (!r.dirty) {
          Node(Triangle, Color.cyan)
        } else {
          val _nodes = (0 until r.usedsize) map {
            i =>
              val zbr = r.zbranches(i)
              measure(zbr.adr, zbr.child)
          }
          val nodes = _nodes.toList
          val widths = nodes.map(_.width)
          val heights = nodes.map(_.height)
          Node(Rectangle, fg, nodes, widths.sum, heights.max)
        }
      }
    }
  }

  val scrollPane = new ScrollPane(view)
  val page = tab("Index", scrollPane)

  def apply(flashix: Flashix) {
    node = view.measure(flashix.btree.ADRT, flashix.btree.RT)
    view.revalidate()
    view.repaint()
  }
}