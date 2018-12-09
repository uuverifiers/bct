package bct

import scala.collection.mutable.ListBuffer

class Constraint
case class PositiveConstraint(val equalityDisjunction : List[(Term, Term)]) extends Constraint
case class NegativeConstraint(val disequalityDisjunction : List[(Term, Term)]) extends Constraint

object BlockingConstraints {

  def apply(constraint : Constraint) : BlockingConstraints = BlockingConstraints(List(constraint))

  def fromBlockingClauses(blockingClauses : List[List[(Term, Term)]]) : BlockingConstraints = {
    val constraints = for (bc <- blockingClauses) yield PositiveConstraint(bc)
    BlockingConstraints(constraints)
  }

  val Empty  = BlockingConstraints(List())
}

case class BlockingConstraints(val blockingConstraints : List[Constraint]) {

  def ++(that : BlockingConstraints) = {
    BlockingConstraints(blockingConstraints ++ that.blockingConstraints)
  }

  // TODO: This is for legacy BREU
  def toBlockingClauses() = {
    val posBC = ListBuffer() : ListBuffer[List[(Term, Term)]]
    val negBC = ListBuffer() : ListBuffer[List[(Term, Term)]]    
    val tmp = 
      for (bc <- blockingConstraints) yield {
        bc match {
          case PositiveConstraint(eq) => posBC += eq
          case NegativeConstraint(eq) => negBC += eq
        }
      }
    (posBC.toList, negBC.toList)
  }

}
