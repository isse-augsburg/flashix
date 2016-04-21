package visualization

class ObservableRef[A](a: A) extends helpers.scala.Ref[A](a) with Observable[A] {
  override def set(a: A) {
    super.set(a)
    update(a)
  }
  
  def updateCurrent() = update(a)
}