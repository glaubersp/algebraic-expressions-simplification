package arithExpr

object ArithExpr {
  def eval(p: ArithExpr): Int = {
    p match {
      case Num(v) => v
      case Sum(left, right) => eval(left) + eval(right)
      case Sub(left, right) => eval(left) - eval(right)
      case Prod(left, right) => eval(left) * eval(right)
    }
  }
}

sealed trait ArithExpr

case class Num(value: Int) extends ArithExpr

case class Sum(left: ArithExpr, right: ArithExpr) extends ArithExpr

case class Sub(left: ArithExpr, right: ArithExpr) extends ArithExpr

case class Prod(left: ArithExpr, right: ArithExpr) extends ArithExpr

object ArithExample extends App {
  val value: ArithExpr = Sum(Num(3), Num(4))
  println(ArithExpr.eval(value))
}