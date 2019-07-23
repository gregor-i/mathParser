package mathParser.boolean

sealed abstract class BooleanUnitaryOperator(val symbol: Symbol, val apply: Boolean => Boolean)
case object Not extends BooleanUnitaryOperator(Symbol("!"), !_)

sealed abstract class BooleanBinaryOperator(val symbol: Symbol, val apply: (Boolean, Boolean) => Boolean)
case object And extends BooleanBinaryOperator(Symbol("&"), _ && _)
case object Or extends BooleanBinaryOperator(Symbol("|"), _ || _)
case object Equals extends BooleanBinaryOperator(Symbol("="), _ == _)
case object Unequals extends BooleanBinaryOperator(Symbol("!="), _ != _)
