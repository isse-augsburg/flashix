package visualization.models

import javax.swing._
import visualization._
import visualization.Toolkit._
import java.awt.Graphics
import java.awt.Graphics2D
import scala.swing.ScrollPane
import integration.Flashix
import types._
import types.error._
import helpers.scala.Ref
import java.awt.RenderingHints
import java.awt.Color
import types.node._
import java.awt.Dimension
import java.awt.BasicStroke
import scala.collection.mutable.ListBuffer

object Log extends Tab {
  var lebsize = 1
  var aligned = 1

  /**
   * @constructor
   * @param block   number of the block
   * @param offset  current write offset into the block
   * @param flushed flushed to flash up to this offset
   * @param nodes   nodes and their addresses
   */
  case class LogBlock(val block: Int, var offset: Int, var flushed: Int, var nodes: List[(address, node)])
  val log: ListBuffer[LogBlock] = ListBuffer()

  val rowxoffset = 50
  val rowyoffset = 10
  val rowheight = 30
  val rowpadding = 10

  object view extends JPanel {

    override def getPreferredSize = new Dimension(rowxoffset + bytesToPixel(lebsize), log.size * (rowheight + rowpadding))

    def nodeToColor(nd: node): Color = nd match {
      case _: inodenode =>  Color.gray
      case _: dentrynode => Color.blue
      case _: datanode => Color.green
      case _: truncnode => Color.red
    }

    def bytesToPixel(bytes: Int): Int = bytes / aligned

    override def paintComponent(_g: Graphics) {
      val g = _g.asInstanceOf[Graphics2D]
      super.paintComponent(g)

      g.setRenderingHint(
        RenderingHints.KEY_TEXT_ANTIALIASING,
        RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

      g.setRenderingHint(
        RenderingHints.KEY_ANTIALIASING,
        RenderingHints.VALUE_ANTIALIAS_ON);

      for (i <- 0 until log.size) {
        val LogBlock(block, _, flushed, adrnds) = log(i)
        val y = rowyoffset + i * (rowheight + rowpadding)
        val fontheight = g.getFontMetrics.getHeight

        g.setColor(Color.black)
        g.drawString(block.toString, 10, y + (rowheight + rowpadding + fontheight) / 2)
        g.setStroke(new BasicStroke(1))
        g.drawRect(rowxoffset - 1, y - 1, bytesToPixel(lebsize), rowheight + 2)
        g.setStroke(new BasicStroke(10))
        g.drawLine(rowxoffset + bytesToPixel(flushed), y - (rowpadding - 2) / 2, rowxoffset + bytesToPixel(flushed), y + rowheight + rowpadding)

        adrnds.foreach { case (adr, node) =>
          g.setColor(nodeToColor(node))
          val x = bytesToPixel(adr.pos)
          val width = bytesToPixel(adr.size)
          g.fillRect(rowxoffset + x, y, width, rowheight)
          g.setColor(Color.black)
          g.setStroke(new BasicStroke(1))
          g.drawRect(rowxoffset + x, y, width, rowheight)
        }
      }
    }
  }

  val scrollPane = new ScrollPane(view)
  val page = tab("Log", scrollPane)

  def apply(flashix: Flashix) {
    val logblocks = flashix.persistence.LOG
    lebsize = flashix.LEB_SIZE
    aligned = encoding.node_header.NODE_HEADER_SIZE(flashix.ops)

    var redraw = false

    // Add/Change blocks
    logblocks.list.foreach { block =>
      val offset = flashix.persistence.LPT(block).size
      val flushed = flashix.wbuf.BUFLEB match {
        case bufleb.nobuffer =>
          offset
        case bufleb.buffered(leb) =>
          if (leb == block)
            flashix.wbuf.WBUF.offset
          else
            offset
      }

      // NOTE: the addresses and nodes only change, when offset changes, too
      val oldblock = log.find { _.block == block }
      val changed = oldblock.map { oldblock => oldblock.offset != offset || oldblock.flushed != flushed }.getOrElse(true)

      if (changed) {
        val adrs = new address_list()
        val nds = new group_node_list()
        val err: Ref[error] = Ref.empty
        flashix.persistence.read_gblock_nodes(block, adrs, nds, err)
        val nodes = adrs.list.zip(nds.list.map { _.nd }).toList
        oldblock match {
          case None =>
            // NOTE: new blocks are only appended at the end
            val logblock = LogBlock(block, offset, flushed, nodes)
            log.append(logblock)
          case Some(block) =>
            block.offset = offset
            block.flushed = flushed
            block.nodes = nodes
        }
      }

      redraw = redraw || changed
    }

    // Remove blocks (Commit / Recovery)
    val remove = log.filter { logblock => ! logblocks.contains(logblock.block) }
    log --= remove
    redraw = redraw || ! remove.isEmpty

    if (redraw) {
      view.revalidate()
      view.repaint()
    }
  }
}
