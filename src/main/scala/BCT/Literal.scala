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

  def copy(suffix : String) : Literal

  def regularityConstraint(that : Literal) : Option[Constraint] = {
    (this, that) match {
      case (PositiveLiteral(a1), PositiveLiteral(a2)) if (a1.predicate == a2.predicate) => Some(NegativeConstraint(a1.args zip a2.args))
      case (NegativeLiteral(a1), NegativeLiteral(a2)) if (a1.predicate == a2.predicate) => Some(NegativeConstraint(a1.args zip a2.args))
      case (PositiveEquation(lhs1, rhs1), PositiveEquation(lhs2, rhs2)) => Some(NegativeConstraint(List((lhs1, lhs2), (rhs1, rhs2))))
      case (NegativeEquation(lhs1, rhs1), NegativeEquation(lhs2, rhs2)) => Some(NegativeConstraint(List((lhs1, lhs2), (rhs1, rhs2))))
      case _ => None
    }
  }
}

case object True extends Literal {
  val terms = Set()
  override def copy(suffix : String) = this
}

case object False extends Literal{
  val terms = Set()
  override def copy(suffix : String) = this
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
  override def copy(suffix : String) = PositiveLiteral(atom.copy(suffix))

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
  override def copy(suffix : String) = NegativeLiteral(atom.copy(suffix))  
}

case class PositiveEquation(lhs : Term, rhs : Term) extends Literal {
  override def toString() = lhs + " = " + rhs
  override val isPositiveEquation = true
  override val terms = Set(lhs, rhs)
  override def copy(suffix : String) = PositiveEquation(lhs.copy(suffix), rhs.copy(suffix))
}

case class NegativeEquation(lhs : Term, rhs : Term) extends Literal {
  override def toString() = lhs + " != " + rhs
  override val isNegativeEquation = true
  override val terms = Set(lhs, rhs)
  override def copy(suffix : String) = NegativeEquation(lhs.copy(suffix), rhs.copy(suffix))  
}
