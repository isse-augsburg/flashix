package visualization

import com.mxgraph.swing.mxGraphComponent
import com.mxgraph.view.mxGraph
import scala.swing.Component

class GraphView(model: mxGraph) extends Component {
  override lazy val peer = {
    val gc = new mxGraphComponent(model)
    gc.setEnabled(false)
    gc
  }
}