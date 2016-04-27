package visualization.models

import integration.Flashix
import visualization._
import visualization.Toolkit._
import scala.swing._
import java.awt.Graphics
import java.awt.Color
import javax.swing.JPanel

object SimpleLPT extends Tab {

  var data: Array[Array[Int]] = null

  object view extends JPanel {
    val colors = Array(Color.green, Color.gray, Color.blue)

    def PADDING = 32

    def dimension = {
      if (data != null) {
        val width = data.length + 2 * PADDING
        val height = 100
        new Dimension(width, height)
      } else {
        super.getPreferredSize
      }
    }

    override def paintComponent(_g: Graphics) {
      val g = _g.asInstanceOf[Graphics2D]
      super.paintComponent(g)

      if (data == null) return

      val width = getWidth - 2 * PADDING
      val height = getHeight() - 2 * PADDING

      val n = data.length
      val d = if (width < n) 1 else width / n

      for (i <- 0 until n) {
        val x = PADDING + i * d
        var y = PADDING
        val entry = data(i)
        val m = entry.length
        val total = entry.sum

        for (j <- 0 until m) {
          g.setColor(colors(j % colors.length))
          val h = entry(j) * height / total
          g.fillRect(x, y, d, h)
          y += h
        }
      }
    }
  }

  val scrollPane = new ScrollPane(view)
  val page = tab("LPT", scrollPane)

  def apply(flashix: Flashix) {
    import flashix.ops._

    val lpt = flashix.persistence.LPT

    data = (0 until lpt.length).toArray map {
      i =>
        val used = lpt(i).ref_size
        val garbage = lpt(i).size - lpt(i).ref_size
        val free = LEB_SIZE - lpt(i).size
        Array(free, garbage, used)
    }

    view.revalidate()
    view.repaint()
  }
}