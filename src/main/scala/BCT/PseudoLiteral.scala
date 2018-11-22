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

}
