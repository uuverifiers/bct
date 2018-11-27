package bct

abstract class Literal {
  def isComplementary(that : Literal) = {
    (this, that) match {
      case (PositiveLiteral(a1), NegativeLiteral(a2)) => a1.predicate == a2.predicate
      case (NegativeLiteral(a1), PositiveLiteral(a2)) => a1.predicate == a2.predicate
      case _ => false
    }
  }

  val isNegativeFlatEquation = false
  val isPositiveFlatEquation = false

  val terms : List[Term]
}

case class PositiveLiteral(atom : Atom) extends Literal {
  override def toString() = atom.toString()
  override val terms = atom.terms
}
case class NegativeLiteral(atom : Atom) extends Literal {
  override def toString() = "(-" + atom + ")"
  override val terms = atom.terms  
}

case class PositiveFlatEquation(lhs : Term, rhs : Term) extends Literal {
  override def toString() = lhs + " = " + rhs
  override val isPositiveFlatEquation = true
  override val terms = List(lhs, rhs)
}

case class NegativeFlatEquation(lhs : Term, rhs : Term) extends Literal {
  override def toString() = lhs + " != " + rhs
  override val isNegativeFlatEquation = true
  override val terms = List(lhs, rhs)  
}
