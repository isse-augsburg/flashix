package visualization

import scala.swing.TabbedPane
import scala.swing.TabbedPane.Page
import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.Label
import scala.swing.Button
import scala.swing.Action
import javax.swing.border.EmptyBorder
import java.awt.Graphics2D
import java.awt.Graphics
import javax.swing.Icon
import java.awt.Color
import java.awt.BasicStroke
import java.awt.Insets

class CloseableTabbedPane extends TabbedPane {
  def setPageClosable(page: Page): Unit = {
    val idx = pages.size - 1
    if (idx >= 0) peer.setTabComponentAt(idx, xForPage(page).peer)
  }

  private def xForPage(page: Page) = new BoxPanel(Orientation.Horizontal) {
    opaque = false
    val title = new Label(page.title)
    title.border = new EmptyBorder(new Insets(0, 5, 0, 10))
    contents += title

    private object closeButton extends Button(Action("Closing") {
      try {
        pages.remove(page.index)
      } catch { case _: Throwable => }
    }) {
      text = ""

      icon = new CloseIcon(false)
      border = new EmptyBorder(new Insets(0, 0, 0, 0))
      contentAreaFilled = false
      rolloverEnabled = true
      rolloverIcon = new CloseIcon(true)
    }
    contents += closeButton
  }

  private class CloseIcon(rollover: Boolean) extends Icon {
    def getIconWidth = 9
    def getIconHeight = 9

    def paintIcon(c: java.awt.Component, g: Graphics, x: Int, y: Int): Unit = {
      val g2 = g.asInstanceOf[Graphics2D]
      if (rollover) {
        g2.setColor(Color.red)
        g2.setStroke(new BasicStroke(3))
      } else {
        g2.setStroke(new BasicStroke(2));
        g2.setColor(Color.gray)
      }
      g2.drawLine(0, 0, getIconWidth - 1, getIconHeight - 1)
      g2.drawLine(getIconWidth - 1, 0, 0, getIconHeight - 1)
    }
  }
}