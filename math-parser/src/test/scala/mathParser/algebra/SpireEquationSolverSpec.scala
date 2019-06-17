package mathParser.algebra

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class SpireEquationSolverSpec extends FunSuite {
  val lang = mathParser.MathParser.doubleLanguage('x, 'y, 'a, 'b, 'c)

  import lang._

  val a = Variable('a)
  val b = Variable('b)
  val c = Variable('c)

  val x = Variable('x)
  val y = Variable('y)
  val z = Variable('z)

  val lhs = a * x + b
  val rhs = c * y

  test("solveability checks the number of occurances of the variable") {
    lang.solveEquation(rhs, rhs, 'x) shouldBe Left("wrong number of ocurrences of 'x'")
    lang.solveEquation(lhs, lhs, 'x) shouldBe Left("wrong number of ocurrences of 'x'")
    lang.solveEquation(lhs, rhs, 'z) shouldBe Left("wrong number of ocurrences of 'z'")
  }

  test("solve this example eq") {
    lang.solveEquation(lhs, rhs, 'x) shouldBe Right((c * y - b) / a)
    lang.solveEquation(lhs, rhs, 'y) shouldBe Right((a * x + b) / c)
  }

  test("solve all binary operations") {
    lang.solveEquation(a + b, c, 'a) shouldBe Right(c - b)
    lang.solveEquation(a + b, c, 'b) shouldBe Right(c - a)

    lang.solveEquation(a - b, c, 'a) shouldBe Right(c + b)
    lang.solveEquation(a - b, c, 'b) shouldBe Right(a - c)

    lang.solveEquation(a * b, c, 'a) shouldBe Right(c / b)
    lang.solveEquation(a * b, c, 'b) shouldBe Right(c / a)

    lang.solveEquation(a / b, c, 'a) shouldBe Right(c * b)
    lang.solveEquation(a / b, c, 'b) shouldBe Right(a / c)

    lang.solveEquation(a ^ b, c, 'a) shouldBe Right(c ^ (one / b))
    lang.solveEquation(a ^ b, c, 'b) shouldBe Right(log(c) / log(a))
  }

  test("solve all unitary operations") {
    lang.solveEquation(neg(a), c, 'a) shouldBe Right(neg(c))
    lang.solveEquation(sin(a), c, 'a) shouldBe Right(asin(c))
    lang.solveEquation(cos(a), c, 'a) shouldBe Right(acos(c))
    lang.solveEquation(tan(a), c, 'a) shouldBe Right(atan(c))
    lang.solveEquation(asin(a), c, 'a) shouldBe Right(sin(c))
    lang.solveEquation(acos(a), c, 'a) shouldBe Right(cos(c))
    lang.solveEquation(atan(a), c, 'a) shouldBe Right(tan(c))
    lang.solveEquation(exp(a), c, 'a) shouldBe Right(log(c))
    lang.solveEquation(log(a), c, 'a) shouldBe Right(exp(c))

    // currently unsupported:
    lang.solveEquation(sinh(a), c, 'a) shouldBe Left(s"could not transform ${sinh(a)}")
    lang.solveEquation(cosh(a), c, 'a) shouldBe Left(s"could not transform ${cosh(a)}")
    lang.solveEquation(tanh(a), c, 'a) shouldBe Left(s"could not transform ${tanh(a)}")
  }
}
