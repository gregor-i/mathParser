package mathParser.algebra

import mathParser.slices.{AbstractSyntaxTree, Optimize}

trait SpireOptimize[A] extends Optimize{
  _ : AbstractSyntaxTree with SpireOperators[A] with SpireSyntaxSugar[A] =>

  override def optimizationRules: Seq[OptimizationRule] = List(
    replaceConstantsRule,
    {
      case UnitaryNode(Neg, UnitaryNode(Neg, child)) => child
    },
    {
      case BinaryNode(Plus, left, `zero`)  => left
      case BinaryNode(Plus, `zero`, right) => right
    },
    {
      case BinaryNode(Times, `zero`, _) => zero
      case BinaryNode(Times, _, `zero`) => zero
    },
    {
      case BinaryNode(Times, left, `one`)  => left
      case BinaryNode(Times, `one`, right) => right
    },
    {
      case UnitaryNode(Log, UnitaryNode(Exp, child))  => child
    },
    {
      case BinaryNode(Plus, left, UnitaryNode(Neg, child)) => left - child
      case BinaryNode(Minus, left, UnitaryNode(Neg, child)) => left + child
    },
    {
      case BinaryNode(Minus, left, right) if left == right => zero
    },
    {
      case BinaryNode(Divided, left, right) if left == right => one
    },
    {
      case BinaryNode(Divided, `one`, BinaryNode(Divided, `one`, child)) => child
    },
    {
      case BinaryNode(Power, child, ConstantNode(-1.0)) => one / child
    },
  )
}
