package mathParser.slices

trait Evaluate {
  _: LanguageOperators with AbstractSyntaxTree =>

  def evaluate(node: Node)
              (variableAssignment: PartialFunction[Symbol, S]): S =
    node.fold[S](identity,
      (op, child) => op.apply(child),
      (op, left, right) => op.apply(left, right),
      variableAssignment)
}
