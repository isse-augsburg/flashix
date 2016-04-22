package visualization

case class Tree(label: String, isDirty: Boolean, sub: List[Tree] = Nil) {
  def isLeaf = sub.isEmpty
}