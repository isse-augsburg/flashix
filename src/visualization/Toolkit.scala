package visualization

import scala.swing._
import javax.swing.JComponent
import scala.swing.event._
import scala.swing.TabbedPane.Page

object Toolkit {
  implicit def toComponent(c: JComponent) = Component.wrap(c)

  def frame(t: String, c: Component, close: => Unit): Frame = {
    new MainFrame {
      title = t
      contents = c
      visible = true

      reactions += {
        case WindowClosing(_) => close
      }
    }
  }

  def hbox(cs: Component*): Component = {
    new BoxPanel(Orientation.Horizontal) {
      contents ++= cs
    }
  }

  def vbox(cs: Component*): Component = {
    new BoxPanel(Orientation.Vertical) {
      contents ++= cs
    }
  }
  
  def label(text: String): Component = {
    new Label(text) {
    }
  }

  def button(label: String, click: => Unit): Component = {
    new Button(label) {
      reactions += {
        case ButtonClicked(_) => click
      }
    }
  }

  def check(label: String, s: Boolean, toggle: Boolean => Unit): Component = {
    new CheckBox(label) {
      this.selected = s
      reactions += {
        case ButtonClicked(_) => toggle(selected)
      }
    }
  }

  def tab(label: String, c: Component): Page = {
    new Page(label, c)
  }

  def tabs(ps: Page*): Component = {
    new TabbedPane {
      pages ++= ps
    }
  }
}