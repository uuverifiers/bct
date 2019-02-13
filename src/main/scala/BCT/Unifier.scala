package bct

import scala.collection.mutable.{Map => MMap}

object Unifier {
  type UnifSubProblem = List[(Term, Term)]
  type UnifProblem = List[List[(Term, Term)]]
  type Unifier = Map[Term, Term]

  def unifyPair(s : Term, t : Term, sub : Unifier = Map()) : Option[Unifier] = {
    // println("unifyPair(" + s + ", " + t + ", " + sub + ")")
    def bottomTerm(tt : Term) : Term = {
      if (sub contains tt)
        bottomTerm(sub(tt))
      else
        tt
    }

    val newS = bottomTerm(s)
    val newT = bottomTerm(t)
    // println("\tnewS: " + newS)
    // println("\tnewT: " + newT)
    

    if (newS == newT) {
      Some(sub)
    } else if (newS.isUniversal) {
      Some(sub + (s -> t))
    } else if (newT.isUniversal) {
      Some(sub + (t -> s))
    } else {
      None
    }
  }

  def trySubGoal(subGoal : UnifSubProblem, sub : Unifier = Map()) : Option[Unifier] = {
    subGoal match {
      case Nil => Some(sub)
      case (s,t) :: tail => {
        unifyPair(s, t, sub) match { // 
          case Some(newSub) => trySubGoal(tail, newSub)
          case None => None
        }
      }
    }
  }

  def unify(prob : List[UnifProblem]) : Option[Unifier] = {
    assert(prob.length == 1)
    val problem = prob.head

    // One of the goals must be satisfied
    for (goal <- problem) {
      // println("\tGOALS")
      // println("\t\t" + goal.mkString(" & "))
      for (subgoal <- goal) {
        // println("\t\t\t" + subgoal)        
        trySubGoal(goal) match {
          case Some(unif) => return Some(unif)
          case None => ()
        }
      }
    }
    None
  }
}
