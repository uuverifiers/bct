package bct

import scala.collection.mutable.ListBuffer


object Constraint {
  type Clause = List[(Term, Term)]
}

import Constraint.Clause

abstract class Constraint(clause : Clause) {
  val c = clause
  def instantiate(model : Model) : Constraint
}

case class UnificationConstraint(val equalityClause : Clause)
    extends Constraint(equalityClause) {

  def instantiate(model : Model) : UnificationConstraint = {
    UnificationConstraint(for ((s,t) <- equalityClause) yield {
      (model.par(s), model.par(t))
    })
  }
}

case class DisunificationConstraint(val disequalityClause : Clause)
    extends Constraint(disequalityClause) {
  
  def instantiate(model : Model) : DisunificationConstraint = {
    DisunificationConstraint(for ((s,t) <- disequalityClause) yield {
      (model.par(s), model.par(t))
    })
  }
}
