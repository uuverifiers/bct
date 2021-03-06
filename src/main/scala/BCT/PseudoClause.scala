package bct

object PseudoClause {
  val Empty = PseudoClause(List(), List(), Order.Empty)
}

case class PseudoClause(val funEquations : List[FunEquation], literals : List[Literal], val order : Order) extends Iterable[Literal] {
  override def toString() =
    (if (funEquations.isEmpty)
      ""
    else "[" + funEquations.mkString("^") + " :: ") + literals.mkString(" v ") + "]" + " @ " + order

  def apply(i : Int) = PseudoLiteral(funEquations, literals(i))

  val isUnit = literals.length == 1
  val length = literals.length
  def iterator = literals.iterator
  val isPositive = literals.forall(_.isPositive)
  val isNegative = literals.forall(_.isNegative)  

  def copy(suffix : String) = {
    PseudoClause(funEquations.map(_.copy(suffix)), literals.map(_.copy(suffix)), order.copy(suffix))
  }

  def toPseudoLiterals() : List[PseudoLiteral] = {
    val lits = 
      for (l <- literals) yield {
        PseudoLiteral(funEquations, l)
      }

    lits
  }
}
