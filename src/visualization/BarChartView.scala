package visualization

import java.awt._
import javax.swing._

class BarChartView() extends JPanel with Observer[Array[Array[Int]]] {
  val colors = Array(Color.green, Color.gray, Color.blue)

  def PADDING = 32

  var data: Array[Array[Int]] = null

  // override def preferredSize = dimension

  def dimension = {
    if (data != null) {
      val width = data.length + 2 * PADDING
      val height = 100 
      new Dimension(width, height)
    } else {
      super.getPreferredSize
    }
  }

  def apply(in: Array[Array[Int]]) {
    data = in
    revalidate()
    repaint()
  }

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
    val width = getWidth - 2 * PADDING
    val height = getHeight() - 2 * PADDING

    val n = data.length
    val d = if(width < n) 1 else width / n

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