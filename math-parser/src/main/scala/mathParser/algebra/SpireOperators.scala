package mathParser
package algebra

sealed abstract class SpireUnitaryOperator(val symbol: Symbol)
case object Neg extends SpireUnitaryOperator(Symbol("-"))
case object Sin extends SpireUnitaryOperator(Symbol("sin"))
case object Cos extends SpireUnitaryOperator(Symbol("cos"))
case object Tan extends SpireUnitaryOperator(Symbol("tan"))
case object Asin extends SpireUnitaryOperator(Symbol("asin"))
case object Acos extends SpireUnitaryOperator(Symbol("acos"))
case object Atan extends SpireUnitaryOperator(Symbol("atan"))
case object Sinh extends SpireUnitaryOperator(Symbol("sinh"))
case object Cosh extends SpireUnitaryOperator(Symbol("cosh"))
case object Tanh extends SpireUnitaryOperator(Symbol("tanh"))
case object Exp extends SpireUnitaryOperator(Symbol("exp"))
case object Log extends SpireUnitaryOperator(Symbol("log"))

sealed abstract class SpireBinaryOperator(val symbol: Symbol)
case object Plus extends SpireBinaryOperator(Symbol("+"))
case object Minus extends SpireBinaryOperator(Symbol("-"))
case object Times extends SpireBinaryOperator(Symbol("*"))
case object Divided extends SpireBinaryOperator(Symbol("/"))
case object Power extends SpireBinaryOperator(Symbol("^"))

