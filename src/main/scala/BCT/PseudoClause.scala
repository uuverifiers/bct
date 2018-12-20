package bct

object PseudoClause {
  def apply(lit : Literal) : PseudoClause =
    // TODO: Order here is empty... 
    PseudoClause(List(), List(lit), Order.Empty)

  val EmptyPseudoClause = PseudoClause(List(), List(), Order.Empty)
}

case class PseudoClause(val funEquations : List[FunEquation], literals : List[Literal], val order : Order) extends Iterable[Literal] {
  override def toString() =
    (if (funEquations.isEmpty)
      ""
    else "[" + funEquations.mkString("^") + " :: ") + literals.mkString(" v ") + "]" + " @ " + order

  def apply(i : Int) = PseudoLiteral(funEquations, literals(i))

  val length = literals.length
  def iterator = literals.iterator

  def copy(suffix : String) = {
    PseudoClause(funEquations.map(_.copy(suffix)), literals.map(_.copy(suffix)), order.copy(suffix))
  }

  def +(that : PseudoClause) = {
    throw new Exception("Not implemented")    
    // PseudoClause(this.pseudoLiterals ++ that.pseudoLiterals)
  }


  def toPseudoLiterals() : List[PseudoLiteral] = {
    val lits = 
      for (l <- literals) yield {
        PseudoLiteral(funEquations, l)
      }

    lits
  }

  // def extend(pseudoLiteral : PseudoLiteral) : PseudoClause = extend(List(pseudoLiteral))
}
