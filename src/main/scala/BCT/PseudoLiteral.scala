package bct



object PseudoLiteral {
  def apply(funs : List[FunEquation], lit : Literal) = {
    new PseudoLiteral(funs, lit)
  }
  def apply(lit : Literal) = {
    new PseudoLiteral(List(), lit)
  }
}

class PseudoLiteral (val funs : List[FunEquation], val lit : Literal)  {
  override def toString = {
    if (funs.isEmpty)
      "{" + lit + "}"
    else
    "{" + funs.mkString("^") + "::" + lit + "}"
  }

  val funEquations = funs

  def atom = {
    lit match {
      case PositiveLiteral(a) => a
      case NegativeLiteral(a) => a
      case _ => throw new Exception("Retrieving atom from " + lit)
    }
  }

  def terms = {
    lit match {
      case NegativeFlatEquation(lhs, rhs) => (lhs, rhs)
      case PositiveFlatEquation(lhs, rhs) => (lhs, rhs)
      case _ => throw new Exception("Retrieving terms from " + lit)        
    }
  }

  def isComplementary(that : PseudoLiteral) = {
    lit.isComplementary(that.lit)
  }
}
