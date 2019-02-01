package polynomials

import polynomials.Polynomial.{Polynomial, multByFactor, multByPoly, printPoly}

import scala.util.parsing.combinator.RegexParsers

/**
  * https://www.schoolofhaskell.com/user/Sam567/computational-physics/beginner-s-tools/polynomials
  */

/**
  * Exemplos:
  * 6
  * 10x + 2x - (3x + 6)/3
  * 18*(2x+2) - 5
  * ((9x + 81)/3 + 27)/3  - 2x
  * 18x + (12x + 10)*(2x+4)/2 - 5x
  * (2x+5) * (x*(9x + 81)/3 + 27)/(1+1+1)  - 2x
  * (2x+5) * (x*(9x^3 + 81)/3 + 27)/(1+1+1)  - 2x
  **/

/**
  * EBNF:
  * poly = pTerm ('+' pTerm | '-' pTerm)*
  * pTerm = pFactor ('*' pFactor | '/' factor | '(' poly ') )*
  * pFactor = simplePoly | '(' poly ')' | '-' pFactor
  * simplePoly = numericTerm | unitTerm
  * numericTerm = number | number unitTerm
  * unitTerm = 'x' | 'x' '^' factor
  * expr = term ('+' term | '-' term)*
  * term = factor ('*' factor | '/' factor)*
  * factor = number | '(' expr ')'
  * number = digit+
  **/

object PolyParser extends RegexParsers {

  // poly = pTerm ('+' pTerm | '-' pTerm)*
  def poly: Parser[Polynomial] = log(pTerm)("POLY TERM TERM") ~ rep(
    log("+" ~ pTerm)("POLY ADD TERM") | log("-" ~ pTerm)("POLY SUB TERM")
  ) ^^ {
    case p ~ list => (p /: list) {
      case (p1, "+" ~ p2) => Polynomial.addPoly(p1)(p2)
      case (p1, "-" ~ p2) => Polynomial.addPoly(p1)(p2.map(x => -x))
    }
  }

  // pTerm = pFactor ('*' pFactor | '/' factor | '(' poly ') )*
  def pTerm: Parser[Polynomial] = pFactor ~ rep(
    "*" ~ pFactor | "/" ~ factor | "(" ~> poly <~ ")"
  ) ^^ {
    case p ~ list => (p /: list) {
      case (p1, "*" ~ p2) => multByPoly(p1)(p2.asInstanceOf[Polynomial])
      case (p1, "/" ~ n) => multByFactor(p1)(1 / n.asInstanceOf[Int].toFloat)
      case (p1, p2) => multByPoly(p1)(p2.asInstanceOf[Polynomial])
    }
  }

  // pFactor = simplePoly | '(' poly ')' | '-' pFactor
  def pFactor: Parser[Polynomial] = log(simplePoly)("SIMPLE POLY TERM") |
    log("(" ~> poly <~ ")")("POLY POS EXPR TERM") |
    log("-" ~ pFactor)("POLY NEG EXPR TERM") ^^ {
      case "-" ~ x => Polynomial.multByFactor(x)(-1)
      case _ ~ x => x
    }

  // simplePoly = numericTerm | unitTerm
  def simplePoly: Parser[Polynomial] = numericTerm | unitTerm

  // numericTerm = number | number unitTerm
  def numericTerm: Parser[Polynomial] = number ~ opt(unitTerm) ^^ {
    case n ~ None => n :: Nil
    case n ~ Some(poly) => Polynomial.multByFactor(poly)(n)
  }

  // unitTerm = 'x' | 'x' '^' factor
  def unitTerm: Parser[Polynomial] = varX ~ opt(varExp ~ factor) ^^ {
    case _ ~ None => 0 :: 1 :: Nil
    case _ ~ Some(_ ~ e) => List.fill(e)(0) ++ (1 :: Nil)
  }

  def varX: Parser[String] =
    log(
      """x""".r ^^ {
        _.toString
      })("VAR TERM")

  def varExp: Parser[String] =
    log(
      """\^""".r ^^ {
        _.toString
      })("EXPON TERM")

  // expr = term ('+' term | '-' term)*
  def expr: Parser[Int] = log(term)("EXPR TERM") ~ rep(
    log("+" ~ term)("PLUS TERM") | log("-" ~ term)("MINUS TERM")
  ) ^^ {
    case number ~ list => list.foldLeft(number) {
      case (x, "+" ~ y) => x + y
      case (x, "-" ~ y) => x - y
    }
  }

  // term = factor ('*' factor | '/' factor)*
  def term: Parser[Int] = log(factor)("TERM TERM") ~ rep(
    log("*" ~ factor)("MULT TERM") | log("/" ~ factor)("DIV TERM")
  ) ^^ {
    case number ~ list => (number /: list) {
      case (x, "*" ~ y) => x * y
      case (x, "/" ~ y) => x / y
    }
  }

  // factor = number | '(' expr ')'
  def factor: Parser[Int] = log(number)("NUM TERM") | log("(" ~> expr <~ ")")("NUM EXPR TERM")

  // number = digit+
  def number: Parser[Int] =
    log(
      """\d+""".r ^^ {
        _.toInt
      })("NUMBER")

}

object PolyParserExample extends App {

  val parser = PolyParser
  val stdin = io.Source.stdin.getLines().toList

  for (input <- stdin.tail) {
    val result = parser.parseAll(parser.poly, input)
    println(printPoly(result.get))
  }

}


