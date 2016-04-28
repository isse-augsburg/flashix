package visualization.models

import java.awt.BasicStroke
import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.RenderingHints
import scala.swing.ScrollPane
import integration.Flashix
import javax.swing.JPanel
import visualization.Tab
import visualization.Toolkit.tab
import visualization.Toolkit.toComponent
import types.wlstatus._

object WL extends Tab {
  case class Entry(ec: Int, status: wlstatus)

  var data: Array[Entry] = null
  var rows: Int = _
  var cols: Int = _

  def PADDING = 32
  def WIDTH = 28
  def HEIGHT = 28

  object view extends JPanel {
    def interpolate(scale: Int, min: Int, cur: Int, max: Int) = {
      scale * (cur - min) / (max - min)
    }

    override def getPreferredSize = dimension

    def dimension = {
      if (data != null) {
        val width = cols * WIDTH + 2 * PADDING
        val height = rows * HEIGHT + 2 * PADDING
        new Dimension(width, height)
      } else {
        super.getPreferredSize
      }
    }

    override def paintComponent(_g: Graphics) {
      val g = _g.asInstanceOf[Graphics2D]
      super.paintComponent(g)

      if (data == null) return

      g.setRenderingHint(
        RenderingHints.KEY_TEXT_ANTIALIASING,
        RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

      g.setRenderingHint(
        RenderingHints.KEY_ANTIALIASING,
        RenderingHints.VALUE_ANTIALIAS_ON);

      g.setStroke(new BasicStroke(1.5f))

      val width = getWidth - 2 * PADDING
      val height = getHeight() - 2 * PADDING

      val min = data.minBy(_.ec)
      val max = data.maxBy(_.ec)

      for (x <- 0 until cols; y <- 0 until rows) {
        val i = y * cols + x
        if (i < data.length) {
          val wle = data(i)
          val nx = PADDING + x * WIDTH
          val ny = PADDING + y * HEIGHT

          val c = interpolate(512, min.ec, wle.ec, max.ec)
          val red = if (c < 256) c else 255
          val green = if (c <= 256) 255 else 512 - c

          if (wle.status == used) {
            g.setColor(new Color(red, green, 0))
            g.fillRect(nx, ny, WIDTH - 4, HEIGHT - 4)
          }

          if (wle.status == erroneous) {
            g.setColor(Color.red)
            g.drawLine(nx, ny, nx + WIDTH - 4, ny + HEIGHT - 4)
            g.drawLine(nx + WIDTH - 4, ny, nx, ny + HEIGHT - 4)
          } else {
            g.setColor(Color.black)
            g.drawString("" + wle.ec, nx + 4, ny + HEIGHT / 2 + 4)
          }

          if (wle.status == erasing)
            g.setColor(Color.red)

          g.drawRect(nx, ny, WIDTH - 4, HEIGHT - 4)
        }
      }
    }
  }

  val scrollPane = new ScrollPane(view)
  val page = tab("Wear Leveling", scrollPane)

  def apply(flashix: Flashix) {
    data = flashix.ubi.WLARRAY.array.map {
      wle => Entry(wle.ec, wle.status)
    }

    val length = data.length
    rows = Math.sqrt(length).toInt
    cols = (length + rows - 1) / rows
    view.revalidate()
    view.repaint()
  }
}