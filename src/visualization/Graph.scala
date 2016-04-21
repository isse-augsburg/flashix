package visualization

case class Graph[T](nodes:Set[T], edges:Set[(T,T)]) {
  def --(that: Graph[T]): Graph[T] = {
    Graph(this.nodes -- that.nodes, this.edges -- that.edges)
  }
  
  def ++(that: Graph[T]): Graph[T] = {
    Graph(this.nodes ++ that.nodes, this.edges ++ that.edges)
  }
  
  def map[S,R](f: T => S, g: ((T, T)) => R) = {
    (nodes map f, edges map g)
  }
}

object Graph {
  def empty[T] = Graph[T](Set(), Set())
}