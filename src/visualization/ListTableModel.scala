package visualization

import javax.swing.table.AbstractTableModel

class ListTableModel[T <: AnyRef](val columnNames: List[String]) extends AbstractTableModel with Observer[List[List[T]]] {
  var data: List[List[T]] = Nil

  def apply(data: List[List[T]]) {
    this.data = data
    fireTableDataChanged()
  }

  def getColumnCount = columnNames.length
  def getRowCount = data.length
  override def getColumnName(col: Int) = columnNames(col)
  def getValueAt(row: Int, col: Int) = data(row)(col)
}