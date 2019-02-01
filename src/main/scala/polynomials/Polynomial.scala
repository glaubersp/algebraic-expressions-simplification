package polynomials

import scala.annotation.tailrec

object Polynomial {
  type Polynomial = List[Int]

  def printPoly(poly: Polynomial): String = {
    val revPoly = poly reverse

    @tailrec
    def recPrint(polyRec: Polynomial, exp: Int, result: StringBuilder): String = {
      if (polyRec isEmpty) {
        result.toString().trim
      } else {
        val term = new StringBuilder()
        val coef = polyRec.head

        if (coef != 0) {
          val printCoef = coef match {
            case -1 => "-"
            case 1 => " "
            case n => coef.toString
          }

          if (exp == 0) {
            term.append(printCoef)
          } else if (exp == 1) {
            term.append(printCoef + "x")
          } else {
            term.append(printCoef + "x^" + exp)
          }

          if (result.isEmpty) {
            result.append(term)
          } else {
            if (term.nonEmpty && !term.charAt(0).equals('-')) {
              result.append(" + ").append(term)
            } else {
              result.append(" - ").append(term.drop(1))
            }
          }
        }
        recPrint(polyRec.tail, exp - 1, result)
      }
    }

    recPrint(revPoly, revPoly.size - 1, new StringBuilder)
  }

  def addPoly(poly1: Polynomial)(poly2: Polynomial): Polynomial = {
    val diff = poly1.length - poly2.length

    def sumTuple(x: Int, y: Int) = x + y

    if (diff >= 0) {
      poly1 zip (poly2 ++ List.fill(diff)(0)) map (sumTuple _).tupled
    } else {
      poly2 zip (poly1 ++ List.fill(-diff)(0)) map (sumTuple _).tupled
    }
  }

  def multByFactor(poly1: Polynomial)(factor: Float): Polynomial = {
    poly1 map (x => (x * factor).toInt)
  }

  def multByVar(poly: Polynomial): Polynomial = 0 :: poly

  def multByPoly(poly1: Polynomial)(poly2: Polynomial): Polynomial = poly1 match {
    case Nil => Nil
    case p :: p1 => {
      val pXp2 = multByFactor(poly2)(p)
      val xXp1Xp2 = multByVar(multByPoly(p1)(poly2))
      addPoly(pXp2)(xXp1Xp2)
    }
  }
}
