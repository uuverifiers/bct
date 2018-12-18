package bct

object PseudoClause {
  def apply(pl : PseudoLiteral) : PseudoClause =
    PseudoClause(List(pl))

  val EmptyPseudoClause = PseudoClause(List())
}

case class PseudoClause(pseudoLiterals : List[PseudoLiteral]) extends Iterable[PseudoLiteral] {
  override def toString() = "[" + pseudoLiterals.mkString(" v ") + "]"

  def apply(i : Int) = pseudoLiterals(i)

  val length = pseudoLiterals.length
  def iterator = pseudoLiterals.iterator

  def copy(suffix : String) = {
    PseudoClause(pseudoLiterals.map(_.copy(suffix)))
  }

  def +(that : PseudoClause) = {
    PseudoClause(this.pseudoLiterals ++ that.pseudoLiterals)
  }

  // def extend(pseudoLiteral : PseudoLiteral) : PseudoClause = extend(List(pseudoLiteral))
}
