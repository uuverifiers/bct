package bct

//
// Copyright 2018
// peter.backeman@it.uu.se
//

abstract class Literal {
  def isComplementary(that : Literal) = false
  val isNegativeEquation = false
  val isPositiveEquation = false
  val terms : Set[Term]
}

case class PositiveLiteral(atom : Atom) extends Literal {
  override def toString() = atom.toString()
  override val terms = atom.terms
  override def isComplementary(that : Literal) = {
    that match {
      case NegativeLiteral(a) => a.predicate == atom.predicate
      case _ => false
    }
  }
}

case class NegativeLiteral(atom : Atom) extends Literal {
  override def toString() = "(-" + atom + ")"
  override val terms = atom.terms

  override def isComplementary(that : Literal) = {
    that match {
      case PositiveLiteral(a) => a.predicate == atom.predicate
      case _ => false
    }
  }
}

case class PositiveEquation(lhs : Term, rhs : Term) extends Literal {
  override def toString() = lhs + " = " + rhs
  override val isPositiveEquation = true
  override val terms = Set(lhs, rhs)
}

case class NegativeEquation(lhs : Term, rhs : Term) extends Literal {
  override def toString() = lhs + " != " + rhs
  override val isNegativeEquation = true
  override val terms = Set(lhs, rhs)  
}
