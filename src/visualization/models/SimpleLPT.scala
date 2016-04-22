package visualization.models

import integration.Flashix
import visualization._
import visualization.Toolkit._
import javax.swing._
import java.awt._

object SimpleLPT extends Tab {
  def view = new BarChartView()
  val page = tab("LPT", view)

  def apply(flashix: Flashix) {
    import flashix.ops._

    val lpt = flashix.persistence.LPT

    for (i <- 0 until lpt.length) {
      val used = lpt(i).ref_size
      val garbage = lpt(i).size - lpt(i).ref_size
      val free = LEB_SIZE - lpt(i).size
      val lp = LProps(used, garbage, free)
    }
  }
}