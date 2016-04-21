package visualization.models

import scala.swing.Component
import org.jfree.data.general.DefaultPieDataset
import org.jfree.chart.ChartFactory
import org.jfree.chart.plot.PiePlot3D
import org.jfree.util.Rotation
import org.jfree.chart.ChartPanel
import visualization.Tab
import integration.Flashix
import visualization.Toolkit._
import types.lpropflags._
import java.awt.Color

object Space extends Tab {

  private val dataset = new DefaultPieDataset()
  private val chart = ChartFactory.createPieChart3D(null, dataset, true, true, false)
  private val plot = chart.getPlot().asInstanceOf[PiePlot3D]
  plot.setStartAngle(290)
  plot.setDirection(Rotation.CLOCKWISE)
  plot.setForegroundAlpha(0.75f)
  plot.setSectionPaint("free", Color.green)
  plot.setSectionPaint("data", Color.blue)
  plot.setSectionPaint("index", Color.gray)
  plot.setSectionPaint("dark", Color.black)

  private val chartPanel = new ChartPanel(chart)
  chartPanel.setPreferredSize(new java.awt.Dimension(200, 200))
  
  def apply(flashix: Flashix) {
    import flashix.ops._

    val lpt = flashix.persistence.LPT
    var free, data, index, dark = 0
    lpt.array.foreach { entry =>
      entry.flags match {
        case LP_FREE =>
          free += LEB_SIZE
        case LP_GROUP_NODES =>
          data += entry.ref_size
          dark += LEB_SIZE - entry.ref_size
        case LP_INDEX_NODES =>
          index += entry.ref_size
          dark += LEB_SIZE - entry.ref_size
      }
    }
    dataset.setValue("free", free)
    dataset.setValue("data", data)
    dataset.setValue("index", index)
    dataset.setValue("dark", dark)
  }

  val page = tab("Space", chartPanel)
}
