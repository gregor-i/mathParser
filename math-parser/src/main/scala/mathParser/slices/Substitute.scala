package mathParser.slices

trait Substitute {
  _: AbstractSyntaxTree =>

  def substitute(tree: Node, search: Node, replace: Node): Node = {
    def loop(tree: Node): Node =
      tree match {
        case `search` => replace
        case BinaryNode(op, childLeft, childRight) => BinaryNode(op, loop(childLeft), loop(childRight))
        case UnitaryNode(op, child) => UnitaryNode(op, loop(child))
        case unmatched => unmatched
      }

    loop(tree)
  }
}
