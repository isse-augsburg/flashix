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

object LPT extends Tab {

  private val dataset = new DefaultCategoryDataset()
  private val chart = ChartFactory.createStackedBarChart(null, null, "Bytes", dataset, PlotOrientation.VERTICAL, true, true, false)
  chart.setBorderVisible(false)
  private val plot = chart.getPlot.asInstanceOf[CategoryPlot]
  plot.getDomainAxis.setVisible(false)
  plot.getDomainAxis.setCategoryMargin(0)
  private val chartPanel = new ChartPanel(chart)

  def apply(flashix: Flashix) {
    import flashix.ops._

    val lpt = flashix.persistence.LPT
    for (i <- 0 until lpt.length) {
      dataset.setValue(lpt(i).ref_size, "referenced", i)
      dataset.setValue(lpt(i).size - lpt(i).ref_size, "unreferenced", i)
      dataset.setValue(LEB_SIZE - lpt(i).size, "unused", i)
    }
  }

  val page = tab("Blocks", chartPanel)
}
