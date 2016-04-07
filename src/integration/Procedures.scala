package integration

class Procedures extends proc.Procedures {
  override def debug(str: String)(implicit _algebraic_implicit: algebraic.Algebraic) = {
    println(str)
    System.out.flush
  }
}
