package visualization.models

import scala.swing.Component
import visualization.Tab
import integration.Flashix
import visualization.Toolkit._
import types.lpropflags._
import java.awt.Color
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.chart.ChartFactory
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.ChartPanel
import org.jfree.chart.plot.CategoryPlot
import org.jfree.chart.renderer.category.BarRenderer

object LPT extends Tab {

  private val dataset = new DefaultCategoryDataset()
  private val chart = ChartFactory.createStackedBarChart(null, null, "Bytes", dataset, PlotOrientation.VERTICAL, true, true, false)
  chart.setBorderVisible(false)
  private val plot = chart.getPlot.asInstanceOf[CategoryPlot]
  plot.getDomainAxis.setVisible(false)
  plot.getDomainAxis.setCategoryMargin(0)
  plot.getDomainAxis.setLowerMargin(0)
  plot.getDomainAxis.setUpperMargin(0)
  private val renderer = plot.getRenderer.asInstanceOf[BarRenderer]
  renderer.setItemMargin(0)
  renderer.setSeriesPaint(0, Color.blue)
  renderer.setSeriesPaint(1, Color.gray)
  renderer.setSeriesPaint(2, Color.green)
  private val chartPanel = new ChartPanel(chart)

  def apply(flashix: Flashix) {
    import flashix.ops._

    val lpt = flashix.persistence.LPT
    dataset.setNotify(false)
    for (i <- 0 until lpt.length) {
      dataset.setValue(lpt(i).ref_size, "used", i)
      dataset.setValue(lpt(i).size - lpt(i).ref_size, "garbage", i)
      dataset.setValue(LEB_SIZE - lpt(i).size, "free", i)
    }
    dataset.setNotify(true)
  }

  val page = tab("Blocks", chartPanel)
}
