package bct

import org.scalatest._

class UnificationTest extends FunSuite with DiagrammedAssertions {

  test ("Unify") {
    val a = Term("a", 0)
    val b = Term("b", 1)
    val X = Term("X", 2, true)
    val sub = Map(X -> a)

    assert(Unifier.unifyPair(a, a).isDefined)    
    assert(Unifier.unifyPair(X, a).isDefined)
    assert(Unifier.unifyPair(X, b).isDefined)          
    assert(Unifier.unifyPair(X, a, sub).isDefined)

    assert(!Unifier.unifyPair(a, b, sub).isDefined)           
    assert(!Unifier.unifyPair(X, b, sub).isDefined)



    val subProblem1 = List((X,a), (b,X), (X, X))
    val subProblem2 = List((a,a), (a,X), (X, a))    
    assert(!Unifier.trySubGoal(subProblem1).isDefined)
    assert(Unifier.trySubGoal(subProblem2).isDefined)

    assert(Unifier.unify(List(List(subProblem1, subProblem2))).isDefined)
    assert(!Unifier.unify(List(List(subProblem1))).isDefined)    
  }

}
