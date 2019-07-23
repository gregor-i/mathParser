package mathParser.algebra

import mathParser.MathParser
import mathParser.implicits._
import org.scalatest.{FunSuite, Matchers}
import spire.algebra.{Field, NRoot, Trig}

class DeriveSpec extends FunSuite with Matchers {
  case object X
  type V = X.type

  testTemplate(MathParser.doubleLanguage, "double language")
  testTemplate(MathParser.realLanguage, "real language")
  testTemplate(MathParser.complexLanguage, "complex language")

  def testTemplate[A: Field: Trig: NRoot](_lang: SpireLanguage[A, Nothing], langName: String) = {
    val lang = _lang.withVariables[X.type](List(Symbol("x") -> X))

    import lang.{derive, parse}

    test(s"$langName: simple functions") {
      derive(parse("x*x").get)(X) shouldBe parse("1*x+1*x").get
      derive(parse("x + x").get)(X) shouldBe parse("1 + 1").get
      derive(parse("x*x + x").get)(X) shouldBe parse("(1*x+1*x) + 1").get
    }
  }
}
