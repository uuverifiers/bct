package bct

import scala.collection.mutable.ListBuffer

// TODO: Add type for List[(Term, Term)]

abstract class Constraint {
  def instantiate(model : Model) : Constraint

}

case class UnificationConstraint(val equalityDisjunction : List[(Term, Term)])
    extends Constraint {

  val c = equalityDisjunction

  def instantiate(model : Model) : UnificationConstraint = {
    UnificationConstraint(for ((s,t) <- equalityDisjunction) yield {
      (model.par(s), model.par(t))
    })
  }
}
case class DisunificationConstraint(val disequalityDisjunction : List[(Term, Term)])
    extends Constraint {
  val c = disequalityDisjunction
  
  def instantiate(model : Model) : DisunificationConstraint = {
    DisunificationConstraint(for ((s,t) <- disequalityDisjunction) yield {
      (model.par(s), model.par(t))
    })
  }
}

// object BlockingConstraints {
//   def apply(constraint : Constraint) : BlockingConstraints = BlockingConstraints(List(constraint))

//   def fromBlockingClauses(blockingClauses : List[List[(Term, Term)]]) : BlockingConstraints = {
//     val constraints = for (bc <- blockingClauses) yield UnificationConstraint(bc)
//     BlockingConstraints(constraints)
//   }

//   val Empty  = BlockingConstraints(List())
// }

// case class BlockingConstraints(val blockingConstraints : List[Constraint]) {

//   def ++(that : BlockingConstraints) = {
//     BlockingConstraints(blockingConstraints ++ that.blockingConstraints)
//   }

//   def instantiate(model : Model) = 
//     BlockingConstraints(blockingConstraints.map(_.instantiate(model)))

//   // TODO: This is for legacy BREU
//   def toBlockingClauses() = {
//     val posBC = ListBuffer() : ListBuffer[List[(Term, Term)]]
//     val negBC = ListBuffer() : ListBuffer[List[(Term, Term)]]    
//     val tmp = 
//       for (bc <- blockingConstraints) yield {
//         bc match {
//           case UnificationConstraint(eq) => posBC += eq
//           case DisunificationConstraint(eq) => negBC += eq
//         }
//       }
//     (posBC.toList, negBC.toList)
//   }

// }
