package bct

//
// Copyright 2018
// peter.backeman@it.uu.se
//

abstract class Literal {
  def isComplementary(that : Literal) = false
  val isNegativeEquation = false
  val isPositiveEquation = false
  val isAtom = false
  val terms : Set[Term]
  lazy val isPositive : Boolean = throw new Exception("_.isPositive")
  lazy val isNegative : Boolean = throw new Exception("_.isNegative")  

  def copy(suffix : String) : Literal

  def instantiate(model : Model) : Literal

  def regularityConstraint(that : Literal) : Option[DisunificationConstraint] = {
    (this, that) match {
      case (PositiveLiteral(a1), PositiveLiteral(a2)) if (a1.predicate == a2.predicate) => Some(DisunificationConstraint(a1.args zip a2.args))
      case (NegativeLiteral(a1), NegativeLiteral(a2)) if (a1.predicate == a2.predicate) => Some(DisunificationConstraint(a1.args zip a2.args))
      case (PositiveEquation(lhs1, rhs1), PositiveEquation(lhs2, rhs2)) => Some(DisunificationConstraint(List((lhs1, lhs2), (rhs1, rhs2))))
      case (NegativeEquation(lhs1, rhs1), NegativeEquation(lhs2, rhs2)) => Some(DisunificationConstraint(List((lhs1, lhs2), (rhs1, rhs2))))
      case _ => None
    }
  }
}

case object True extends Literal {
  val terms = Set()
  override def copy(suffix : String) = this
  override def instantiate(model : Model) = this
}

case object False extends Literal{
  val terms = Set()
  override def copy(suffix : String) = this
  override def instantiate(model : Model) = this
}


case class PositiveLiteral(atom : Atom) extends Literal {
  override def toString() = atom.toString()
  override val terms = atom.terms
  override val isAtom = true
  override def isComplementary(that : Literal) = {
    that match {
      case NegativeLiteral(a) => a.predicate == atom.predicate
      case _ => false
    }
  }
  override def copy(suffix : String) = PositiveLiteral(atom.copy(suffix))
  override def instantiate(model : Model) = PositiveLiteral(atom.instantiate(model))
  override lazy val isPositive = true
  override lazy val isNegative = false    
}

case class NegativeLiteral(atom : Atom) extends Literal {
  override def toString() = "(~" + atom + ")"
  override val terms = atom.terms
  override val isAtom = true
  override def isComplementary(that : Literal) = {
    that match {
      case PositiveLiteral(a) => a.predicate == atom.predicate
      case _ => false
    }
  }
  override def copy(suffix : String) = NegativeLiteral(atom.copy(suffix))
  override def instantiate(model : Model) = NegativeLiteral(atom.instantiate(model))
  override lazy val isPositive = false
  override lazy val isNegative = true      
}


abstract class Equation(val lhs : Term, val rhs : Term) extends Literal {
  val terms = Set(lhs, rhs)
}

case class PositiveEquation(lhs_ : Term, rhs_ : Term) extends Equation(lhs_, rhs_) {
  override def toString() = lhs + " = " + rhs
  override val isPositiveEquation = true
  override val terms = Set(lhs, rhs)
  override def copy(suffix : String) = PositiveEquation(lhs.copy(suffix), rhs.copy(suffix))
  override def instantiate(model : Model) = PositiveEquation(lhs.instantiate(model), rhs.instantiate(model))
  override lazy val isPositive = true
  override lazy val isNegative = false  
}

case class NegativeEquation(lhs_ : Term, rhs_ : Term) extends Equation(lhs_, rhs_) {
  override def toString() = lhs + " != " + rhs
  override val isNegativeEquation = true
  override val terms = Set(lhs, rhs)
  override def copy(suffix : String) = NegativeEquation(lhs.copy(suffix), rhs.copy(suffix))
  override def instantiate(model : Model) = NegativeEquation(lhs.instantiate(model), rhs.instantiate(model))
  override lazy val isPositive = false
  override lazy val isNegative = true  
}
