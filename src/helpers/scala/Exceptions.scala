// Flashix: a verified file system for flash memory
// (c) 2015-2016 Institute for Software & Systems Engineering <http://isse.de/flashix>
// This code is licensed under MIT license (see LICENSE for details)

package helpers.scala

case class InvalidSelector(msg : String) extends Exception("invalid selector: " + msg) { }
case class InvalidSelectorUpdate(msg : String) extends Exception("invalid selector update: " + msg) { }
case class ChooseFailure() extends Exception("choose failed") { }
