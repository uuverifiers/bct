package bct

import scala.collection.mutable.ListBuffer

class Constraint
case class UnificationConstraint(val equalityDisjunction : List[(Term, Term)]) extends Constraint
case class DisunificationConstraint(val disequalityDisjunction : List[(Term, Term)]) extends Constraint

object BlockingConstraints {
  def apply(constraint : Constraint) : BlockingConstraints = BlockingConstraints(List(constraint))

  def fromBlockingClauses(blockingClauses : List[List[(Term, Term)]]) : BlockingConstraints = {
    val constraints = for (bc <- blockingClauses) yield UnificationConstraint(bc)
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
          case UnificationConstraint(eq) => posBC += eq
          case DisunificationConstraint(eq) => negBC += eq
        }
      }
    (posBC.toList, negBC.toList)
  }

}
