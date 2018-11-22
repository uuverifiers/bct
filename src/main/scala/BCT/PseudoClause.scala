package bct

object PseudoClause {
  def apply(pseudoLiterals : List[PseudoLiteral]) = {
    new PseudoClause(pseudoLiterals)
  }
}

class PseudoClause(pseudoLiterals : List[PseudoLiteral]) extends Iterable[PseudoLiteral] {
  override def toString() = "[" + pseudoLiterals.mkString(" v ") + "]"

  def length = pseudoLiterals.length

  def iterator = pseudoLiterals.iterator
}
