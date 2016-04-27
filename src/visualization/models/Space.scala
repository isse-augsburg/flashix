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
  plot.setSectionPaint("used", Color.blue)

  private val chartPanel = new ChartPanel(chart)
  chartPanel.setPreferredSize(new java.awt.Dimension(200, 200))
  
  def apply(flashix: Flashix) {
    val (total, free) = flashix.computeStats
    dataset.setValue("free", free)
    dataset.setValue("used", total - free)
  }

  val page = tab("Space", chartPanel)
}
