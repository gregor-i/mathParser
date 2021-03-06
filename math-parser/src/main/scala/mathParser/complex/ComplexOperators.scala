package mathParser.complex

sealed abstract class ComplexUnitaryOperator(val name: String)
case object Neg extends ComplexUnitaryOperator("-")
case object Sin extends ComplexUnitaryOperator("sin")
case object Cos extends ComplexUnitaryOperator("cos")
case object Tan extends ComplexUnitaryOperator("tan")
case object Asin extends ComplexUnitaryOperator("asin")
case object Acos extends ComplexUnitaryOperator("acos")
case object Atan extends ComplexUnitaryOperator("atan")
case object Sinh extends ComplexUnitaryOperator("sinh")
case object Cosh extends ComplexUnitaryOperator("cosh")
case object Tanh extends ComplexUnitaryOperator("tanh")
case object Exp extends ComplexUnitaryOperator("exp")
case object Log extends ComplexUnitaryOperator("log")

sealed abstract class ComplexBinaryOperator(val name: String)
case object Plus extends ComplexBinaryOperator("+")
case object Minus extends ComplexBinaryOperator("-")
case object Times extends ComplexBinaryOperator("*")
case object Divided extends ComplexBinaryOperator("/")
case object Power extends ComplexBinaryOperator("^")

