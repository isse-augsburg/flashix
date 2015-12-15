package helpers.scala

class ObservableRef[A](a: A) extends Ref[A](a) {
  type Observer = A => Unit;

  var observers = scala.collection.immutable.Set.empty[Observer]

  override def set(a: A) {
    super.set(a)
    update()
  }

  def update() {
    for(o <- observers)
      o.apply(get)
  }
  
  def +=(o: Observer) {
    observers += o
  }
  
  def -=(o: Observer) {
    observers -= o
  }
}