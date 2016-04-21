package visualization

import javax.swing.table.TableModel
import scala.swing.Table
import scala.swing.Button
import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.ScrollPane
import scala.swing.Dialog
import scala.swing.event.ButtonClicked
import scala.swing.TextField
import scala.swing.BufferWrapper
import scala.swing.TabbedPane.Page
import scala.swing.Component

class TableView(name: String, _model: TableModel) extends Table {
  model = _model
}
