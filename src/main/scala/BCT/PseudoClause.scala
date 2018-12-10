package bct

object PseudoClause {
  def apply(pl : PseudoLiteral) : PseudoClause =
    PseudoClause(List(pl))

  val EmptyPseudoClause = PseudoClause(List())
}

case class PseudoClause(pseudoLiterals : List[PseudoLiteral]) extends Iterable[PseudoLiteral] {
  override def toString() = "[" + pseudoLiterals.mkString(" v ") + "]"

  val length = pseudoLiterals.length
  val iterator = pseudoLiterals.iterator

  def copy(suffix : String) = {
    PseudoClause(pseudoLiterals.map(_.copy(suffix)))
  }
}
