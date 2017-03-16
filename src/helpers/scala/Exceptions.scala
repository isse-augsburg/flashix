package helpers.scala

case class InvalidSelector(msg : String) extends Exception("invalid selector: " + msg) { }
case class InvalidSelectorUpdate(msg : String) extends Exception("invalid selector update: " + msg) { }
case class ChooseFailure() extends Exception("choose failed") { }
