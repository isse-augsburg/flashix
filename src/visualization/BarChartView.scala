package visualization

import java.awt._
import javax.swing._

class BarChartView() extends JPanel with Observer[Array[Array[Int]]] {
  val colors = Array(Color.blue, Color.gray, Color.green)

  var data: Array[Array[Int]] = null

  def apply(in: Array[Array[Int]]) {
    data = in
    revalidate()
    repaint()
  }

  def PADDING = 32

  override def paintComponent(_g: Graphics) {
    val g = _g.asInstanceOf[Graphics2D]
    super.paintComponent(g)

    /*g.setRenderingHint(
      RenderingHints.KEY_TEXT_ANTIALIASING,
      RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

    g.setRenderingHint(
      RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON);*/

    if (data != null)
      paint(g)
  }

  def paint(g: Graphics2D) {
    val width = getWidth()
    val height = getHeight()

    val n = data.length
    val d = 1 // (width - 2 * PADDING) / n

    for (i <- 0 until n) {
      val x = PADDING + i * d
      var y = height - PADDING
      val entry = data(i)
      val m = entry.length
      val total = entry.sum

      for (j <- 0 until m) {
        g.setColor(colors(j % colors.length))
        val h = entry(j) * (height - 2 * PADDING) / total
        val v = if (j == m - 1) PADDING else y - h
        g.fillRect(x, v, x + d, y)
        y -= h
      }
    }
  }
}