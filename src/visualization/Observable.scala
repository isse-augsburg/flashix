package visualization

trait Observable[A] {
  var observers: Set[Observer[A]] = Set()
  def outer = this

  def +=(o: Observer[A]) = observers += o
  def -=(o: Observer[A]) = observers -= o

  def update(a: A) {
    for (o <- observers)
      o.apply(a)
  }
}