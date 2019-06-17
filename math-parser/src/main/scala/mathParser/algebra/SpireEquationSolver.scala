package mathParser.algebra

import mathParser.slices.{AbstractSyntaxTree, FreeVariables}

import scala.annotation.tailrec

trait SpireEquationSolver[A] {
  _: AbstractSyntaxTree with SpireLanguage[A] =>

  def solveEquation(lhs: Node, rhs: Node, variable: Symbol): Either[String, Node] = {

    def countVariable(node: Node): Int =
      node.fold[Int](
        ifConstant = _ => 0,
        ifUnitary = (_, c) => c,
        ifBinary = (_, l, r) => l + r,
        ifVariable = v => if (v == variable) 1 else 0
      )

    def once(node: Node): Boolean = countVariable(node) == 1

    def never(node: Node): Boolean = countVariable(node) == 0

    @tailrec
    def loop(target: Node, remaining: Node): Either[String, Node] =
      target match {
        case BinaryNode(Plus, left, right) if once(left) => loop(left, remaining - right)
        case BinaryNode(Plus, left, right) if once(right) => loop(right, remaining - left)
        case BinaryNode(Minus, left, right) if once(left) => loop(left, remaining + right)
        case BinaryNode(Minus, left, right) if once(right) => loop(right, left - remaining)
        case BinaryNode(Times, left, right) if once(left) => loop(left, remaining / right)
        case BinaryNode(Times, left, right) if once(right) => loop(right, remaining / left)
        case BinaryNode(Divided, left, right) if once(left) => loop(left, remaining * right)
        case BinaryNode(Divided, left, right) if once(right) => loop(right, left / remaining)
        case BinaryNode(Power, left, right) if once(left) => loop(left, remaining ^ (one / right))
        case BinaryNode(Power, left, right) if once(right) => loop(right, log(remaining) / log(left))

        case UnitaryNode(Neg, child) => loop(child, neg(remaining))
        case UnitaryNode(Sin, child) => loop(child, asin(remaining))
        case UnitaryNode(Cos, child) => loop(child, acos(remaining))
        case UnitaryNode(Tan, child) => loop(child, atan(remaining))
        case UnitaryNode(Asin, child) => loop(child, sin(remaining))
        case UnitaryNode(Acos, child) => loop(child, cos(remaining))
        case UnitaryNode(Atan, child) => loop(child, tan(remaining))
        case UnitaryNode(Exp, child) => loop(child, log(remaining))
        case UnitaryNode(Log, child) => loop(child, exp(remaining))

        case Variable(name) if name == variable => Right(remaining)
        case unmatched => Left(s"could not transform $unmatched")
      }

    if (once(lhs) && never(rhs))
      loop(lhs, rhs)
    else if (once(rhs) && never(lhs))
      loop(rhs, lhs)
    else
      Left(s"wrong number of ocurrences of '${variable.name}'")
  }


}
