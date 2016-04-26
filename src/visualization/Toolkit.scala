package visualization

import scala.swing._
import javax.swing.JComponent
import scala.swing.event._
import scala.swing.TabbedPane.Page

object Toolkit {
  implicit def toComponent(c: JComponent) = Component.wrap(c)

  def frame(t: String, c: Component, k: => Unit) = {
    new MainFrame {
      title = t
      contents = c
      visible = true

      override def closeOperation() { k }
    }
  }

  def hbox(cs: Component*) = {
    new BoxPanel(Orientation.Horizontal) {
      contents ++= cs
    }
  }

  def vbox(cs: Component*) = {
    new BoxPanel(Orientation.Vertical) {
      contents ++= cs
    }
  }

  def label(text: String) = {
    new Label(text) {
    }
  }

  def button(label: String, k: => Unit) = {
    new Button(label) {
      reactions += {
        case ButtonClicked(_) => k
      }
      maximumSize = new Dimension(200, 0)
    }
  }

  def check(label: String, s: Boolean, toggle: Boolean => Unit) = {
    new CheckBox(label) {
      this.selected = s
      reactions += {
        case ButtonClicked(_) => toggle(selected)
      }
    }
  }

  def tab(label: String, c: Component) = {
    new Page(label, c)
  }

  def tabs(ps: Page*) = {
    new TabbedPane {
      pages ++= ps
    }
  }
}