import org.scalatest._
import polynomials.{PolyParser, Polynomial}

class PolyParserTest extends FunSuite {
  override def withFixture(test: NoArgTest) = { // Define a shared fixture
    // Shared setup (run at beginning of each test)
    try test()
    finally {
      // Shared cleanup (run at end of each test)
    }
  }

  test("Set 1") {
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "10x + 2x - (3x + 6)/3").get) == "11x - 2")
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "18*(2x+2) - 5").get) == "36x + 31")
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "((9x + 81)/3 + 27)/3  - 2x").get) == "-x + 18")
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "18x + (12x + 10)*(2x+4)/2 - 5x").get) == "12x^2 + 47x + 20")
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "(2x+5) * (x*(9x + 81)/3 + 27)/(1+1+1)  - 2x").get) == "2x^3 + 23x^2 + 61x + 45")
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "(2x+5) * (x*(9x^3 + 81)/3 + 27)/(1+1+1)  - 2x").get) == "2x^5 + 5x^4 + 18x^2 + 61x + 45")
  }

  test("Set 2") {
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "- (4x + 8)/4 + 10x + 2x").get) == "11x - 2")
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "- 5 + 9*(4x+4)").get) == "36x + 31")
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "-3x + ((9x + 81)/3 + 27)/3").get) == "-2x + 18")
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "- 5x + 18x + (24x + 20)*(2x+4)/4").get) == "12x^2 + 47x + 20")
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "(2x+5) * (x*(3x+27) + 27)/3 - x").get) == "2x^3 + 23x^2 + 62x + 45")
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "(2x+5) * (x*(3x^3 + 27) + 27)/3 - x").get) == "2x^5 + 5x^4 + 18x^2 + 62x + 45")
  }

  test("Set 3") {
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "x + 2x - (3x + 6)/3").get) == "2x - 2")
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "(1+7+10)*(2x+2) - 5").get) == "36x + 31")
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "((9x^5 + 81)/3 + 27)/3  - 2x").get) == "x^5 - 2x + 18")
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "18x^2 + (2x + 10)*(2x+4)/2 - 5x").get) == "20x^2 + 9x + 20")
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "(2x+5) * (x^2*(9x + 81)/3 + 27)/(1+1+1)  - 2x").get) == "2x^4 + 23x^3 + 45x^2 + 16x + 45")
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "(2x+5) * (x*(9x^3 + 81)/3 + 27)/(1+1+1)  - x^3").get) == "2x^5 + 5x^4 - x^3 + 18x^2 + 63x + 45")
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "(2x+5) * (x*(9x^3 + 81)/3 + 27)/(1+1+1) - x^3 + 2x - (3x + 6)/3").get) == "2x^5 + 5x^4 - x^3 + 18x^2 + 64x + 43")
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "((9x^5 + 81)/3 + 27)/3  - 2x + 18x^2 + (2x + 10)*(2x+4)/2 - 5x").get) == "x^5 + 20x^2 + 7x + 38")
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "18x^2 + (2x + 10)*(2x+4)/2 - 5x + 20x^2 + 30x(2x+1)").get) == "100x^2 + 39x + 20")
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "15x(2x+1) + (2x+5) * (x^2*(9x + 81)/3 + 27)/3  - 2x").get) == "2x^4 + 23x^3 + 75x^2 + 31x + 45")
  }

  test("Set 4") {
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "10x/(1+8/(1+2/(1+1)))").get) == "2x")
    assert(Polynomial.printPoly(PolyParser.parseAll(PolyParser.poly, "24x^2/(2+8/(1+2/(1+1)))").get) == "4x^2")
  }

}