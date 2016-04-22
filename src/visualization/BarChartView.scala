package visualization

import java.awt._
import javax.swing._

case class LProps(used: Int, garbage: Int, free: Int)

class BarChartView extends JPanel {
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
  }
}