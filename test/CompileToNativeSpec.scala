package mathParser

import mathParser.complex.{ComplexCompile, ComplexLanguage}
import mathParser.double.{DoubleCompile, DoubleLanguage}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import spire.math.Complex

class CompileToNativeSpec extends Specification with ScalaCheck {
  private val variable = 'x
  private val someFunctions = Gen.oneOf(
    "x^(3+10)",
    "x^(3+10) -1",
    "x^(10) -1",
    "x^(3) -1",
    "x^(x) -1",
    "x^(x) +1",
    "x^(2) +x^(2) +1",
    "x^(-x) +1",
    "x*x*x - 5",
    "x*x*x+5",
    "x^3-x^2+x-1",
    "sin(x) + x^3+1",
    "cos(x)+1",
    "cos(x)-1",
    "x*x*x+5",
    "x*x*x*x+5",
    "sin(x) + (x+1)*(x-1)+5",
    "x^x - 0.99",
    "x^x*(x+2)",
    "(x+2)*(x-0.5)*(x+log(x))*(x-5)",
    "(x+2)*(x-0.5)*(x+sin(x))*(x-2)*(x+2)*(x-0.5)*(x+1)*(x-2)",
    "(x+2)*(x-0.5)*(x+sin(x))*(x-2)*(x-0.5)*(x+3)",
    "sin(x)*sin(5*x)",
    "sin(x - 1)*sin(3*x) + 0.1",
    "sin(x - 1)*sin(-5*x) - 0.1",
    "x^sin(x) + 1",
    "x^sin(x) - 1 - sin(x)^x + 1",
    "sin(x)*cos(5*x) + 0.5",
    "sin(x*10)*cos(9*x*10) + 0.5",
    "sin(x*5)*cos(11*x*5)",
    "sin(5*x*5)*cos(x*5) + 0.5",
    "e^-(x*x) + e^(-5*x*x)",
    "sin(x)^cos(x) - 0.1",
    "x^(25+x)",
    "e^x - 5",
    "e^(3*x) - 9*0.5",
    "e^(x*x) - 5*0.5",
    "e^(x+x*x) - x*0.5",
    "(x-1)^4*(x)^4",
    "35*x^9-180*x^7+378*x^5-420*x^3+315*x",
    "35*x^9-180*x^7+3798*x^5-420*x^3+315+x"
  )

  private val doubleTrees = {
    val parser = new Parser[DoubleLanguage.type](DoubleLanguage, Set(variable), DoubleLanguage.literalParser)
    someFunctions.map(parser(_).get)
  }
  private val complexTrees = {
    val parser = new Parser[ComplexLanguage.type](ComplexLanguage, Set(variable), ComplexLanguage.literalParser)
    someFunctions.map(parser(_).get)
  }

  private val genComplex: Gen[Complex[Double]] = for {
    imag <- Arbitrary.arbDouble
    real <- Arbitrary.arbDouble
  } yield Complex(imag, real)

  implicit class DoubleEquality(left: Double) {
    def ===(right: Double): Boolean = (left == right) || (left != left && right != right)
  }

  "'Compile to Native' for the double language with parameter 'x'" >> {
    forAll(doubleTrees) {
      term =>
        val f = DoubleCompile(variable)(term).get
        forAll {
          x: Double => f(x) === Evaluate(term)({ case 'x => x })
        }
    }
  }

  "'Compile to Native' for the complex language with parameter 'x'" >> {
    forAll(complexTrees) {
      term =>
        val f = ComplexCompile(variable)(term).get
        forAll(genComplex) {
          x => f(x) === Evaluate(term)({ case 'x => x })
        }
    }
  }
}
