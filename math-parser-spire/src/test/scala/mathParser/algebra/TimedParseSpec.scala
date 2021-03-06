package mathParser.algebra

import mathParser.{LiteralParser, MathParser, Parser, SpireLanguages}
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.{FunSuite, Matchers}
import spire.algebra.{Field, NRoot, Trig}
import mathParser.SpireImplicits._
import org.scalatest.time.{Millis, Span}


class TimedParseSpec extends FunSuite with Matchers with TimeLimitedTests {

  val timeLimit = Span(200, Millis)

  sealed trait V
  case object A extends V
  case object X extends V

  def vList = List(
    "a" -> A,
    "x" -> X)

  testTemplate(SpireLanguages.doubleLanguage.withVariables[V](vList), "double language")
  testTemplate(SpireLanguages.realLanguage.withVariables[V](vList), "real language")
  testTemplate(SpireLanguages.complexLanguage.withVariables[V](vList), "complex language")

  def testTemplate[A: Field: Trig: NRoot: LiteralParser](lang: SpireLanguage[A, V], langName: String) = {
    test(s"$langName: parse expressions with many parenthesis") {
      val expr = "((((x * x + a) * (x * x + a) + a) * ((x * x + a) * (x * x + a) + a) + a) * (((x * x + a) * (x * x + a) + a) * ((x * x + a) * (x * x + a) + a) + a) + a)"
      lang.parse(expr).isDefined shouldBe true
    }

    test(s"$langName: parse expressions with many operators"){
      val expr =  "35*x*x*x*x*x*x*x*x*x-180*x*x*x*x*x*x*x+378*x*x*x*x*x-420*x*x*x+315+x + a"
      lang.parse(expr).isDefined shouldBe true
    }
  }
}
