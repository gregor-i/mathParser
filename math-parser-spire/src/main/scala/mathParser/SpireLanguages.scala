package mathParser

import mathParser.algebra.SpireLanguage
import mathParser.boolean.{BooleanBinaryOperator, BooleanLanguage, BooleanUnitaryOperator}
import spire.implicits._
import spire.math.{Complex, Real}

object SpireLanguages {
  val doubleLanguage: SpireLanguage[Double, Nothing] =
    SpireLanguage[Double]

  val complexLanguage: SpireLanguage[Complex[Double], Nothing] =
    SpireLanguage[Complex[Double]]
      .addConstant("i", Complex.i[Double])

  val realLanguage: SpireLanguage[Real, Nothing] =
    SpireLanguage[Real]
}
