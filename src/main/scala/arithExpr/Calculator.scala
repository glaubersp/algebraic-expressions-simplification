package arithExpr

import scala.util.parsing.combinator.RegexParsers

/**
  * EBNF:
  * number = digit+
  * factor = number | '(' expr ')'
  * term = factor ('*' factor | '/' factor)*
  * expr = term ('+' term | '-' term)*
  */
object Calculator extends RegexParsers {
  def number: Parser[Int] =
    """\d+""".r ^^ {
      _.toInt
    }

  def factor: Parser[Int] = log(number)("Number") | log("(" ~> expr <~ ")")("Parenthesis")

  def term: Parser[Int] = factor ~ rep("*" ~ log(factor)("Mult term") | "/" ~ log(factor)("Div term")) ^^ {
    case number ~ list => (number /: list) {
      case (x, "*" ~ y) => x * y
      case (x, "/" ~ y) => x / y
    }
  }

  def expr: Parser[Int] = term ~ rep("+" ~ log(term)("Plus term") | "-" ~ log(term)("Minus term")) ^^ {
    case number ~ list => list.foldLeft(number) {
      case (x, "+" ~ y) => x + y
      case (x, "-" ~ y) => x - y
    }
  }

  def apply(input: String): Int = parseAll(expr, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }
}

object CalculatorExample extends App {
  val value: String = "9*8+(21/7)"
  println(Calculator.apply(value))
}