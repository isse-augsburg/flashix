package visualization.models

import integration.Flashix
import visualization._
import visualization.Toolkit._
import scala.swing._

object SimpleLPT extends Tab with Observable[Array[Array[Int]]] {
  val view = new BarChartView()
  
  val scrollPane = new ScrollPane(view)
  val page = tab("LPT", scrollPane)
  
  this += view

  def apply(flashix: Flashix) {
    import flashix.ops._

    val lpt = flashix.persistence.LPT

    val entries = (0 until lpt.length).toArray map {
      i =>
        val used = lpt(i).ref_size
        val garbage = lpt(i).size - lpt(i).ref_size
        val free = LEB_SIZE - lpt(i).size
        Array(free, garbage, used)
    }
    
    update(entries)
  }
}