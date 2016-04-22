package visualization

case class Tree(label: String, sub: List[Tree] = Nil) {
  def isLeaf = sub.isEmpty
}