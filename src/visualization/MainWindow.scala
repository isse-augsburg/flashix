package visualization

import scala.swing.MainFrame
import scala.swing.TabbedPane
import scala.swing.Component
import scala.swing.TabbedPane.Page
import javax.swing.JComponent

object MainWindow extends MainFrame {
  val tabbedPane = new TabbedPane()

  def show {
    this.visible_=(true)
    this.title = "Filesystem"
    this.contents = tabbedPane
  }

  def addPage(title: String, component: Component) {
    // this.maximize()
    tabbedPane.pages += new Page(title, component)
  }

  def addPage(title: String, component: JComponent) {
    addPage(title, Component.wrap(component))
  }
}