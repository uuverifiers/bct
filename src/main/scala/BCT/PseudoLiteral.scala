package bct

object PseudoLiteral {
  def apply(lit_ : Literal) : PseudoLiteral =
    PseudoLiteral(List(), lit_)
}

case class PseudoLiteral (val funEquations : List[FunEquation], val lit : Literal)  {
  override def toString = {
    if (funEquations.isEmpty)
      "{" + lit + "}"
    else
    "{" + funEquations.mkString("^") + "::" + lit + "}"
  }

  // Only used if sort is known!
  lazy val atom : Atom = {
    lit match {
      case PositiveLiteral(a) => a
      case NegativeLiteral(a) => a
      case _ => throw new Exception("Retrieving atom from " + lit)
    }
  }

  // Only used if sort is known!  
  lazy val terms : (Term, Term) = {
    lit match {
      case NegativeEquation(lhs, rhs) => (lhs, rhs)
      case PositiveEquation(lhs, rhs) => (lhs, rhs)
      case _ => throw new Exception("Retrieving terms from " + lit)        
    }
  }

  def isComplementary(that : PseudoLiteral) =
    lit.isComplementary(that.lit)
}
