package bct

class Literal {
  def isComplementary(that : Literal) = {
    (this, that) match {
      case (PositiveLiteral(a1), NegativeLiteral(a2)) => a1.predicate == a2.predicate
      case (NegativeLiteral(a1), PositiveLiteral(a2)) => a1.predicate == a2.predicate
      case _ => false
    }
  }

  val isNegativeFlatEquation = false
  val isPositiveFlatEquation = false  
}

case class PositiveLiteral(atom : Atom) extends Literal {
  override def toString() = atom.toString()
}
case class NegativeLiteral(atom : Atom) extends Literal {
  override def toString() = "(-" + atom + ")"
}

case class PositiveFlatEquation(lhs : Term, rhs : Term) extends Literal {
  override def toString() = lhs + " = " + rhs
  override val isPositiveFlatEquation = true
}

case class NegativeFlatEquation(lhs : Term, rhs : Term) extends Literal {
  override def toString() = lhs + " != " + rhs
  override val isNegativeFlatEquation = true
}

case class FunEquation(fun : String, args : List[Term], res : String) extends Literal {
  override def toString() = fun + "(" + args.mkString(",") + ") = " + res
}
