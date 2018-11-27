package bct

object Branch {
  def apply(pseudoLiterals : List[PseudoLiteral]) = {
    new Branch(pseudoLiterals, Open)
  }

  class Conflict
  case object Open extends Conflict
  case class ComplementaryPair(a1 : Atom , a2 : Atom) extends Conflict
  case class InvalidEquation(t1 : Term, t2 : Term) extends Conflict
}


object BranchOrdering extends Ordering[Branch] {
  def compare(x : Branch, y : Branch) = {
    x.length.compare(y.length)
  }
}



class Branch(pseudoLiterals : List[PseudoLiteral], closed : Branch.Conflict, strong : Boolean = true) {
  assert(pseudoLiterals.length > 0)
  def length = pseudoLiterals.length

  override def toString() = pseudoLiterals.mkString("->")


  lazy val conflicts = {
    val allConflicts = {
      if (strong) {
        // Only allow contradiction involving last one or two nodes        
        val n1 = pseudoLiterals(0)
        val pairConflict : List[Branch.Conflict]  =
          if (pseudoLiterals.length > 1) {
            val n2 = pseudoLiterals(1)
            if (n1.isComplementary(n2)) {
              List(Branch.ComplementaryPair(n1.atom, n2.atom))
            } else {
              List()
            }
          } else {
            List()
          }

        val singleConflict : List[Branch.Conflict] =
          if (n1.lit.isNegativeFlatEquation) {
            val (lhs, rhs) = n1.terms
            List(Branch.InvalidEquation(lhs, rhs))
          } else {
            List()
          }

        pairConflict ++ singleConflict
      } else {
        throw new Exception("Weak BREU is not implemented")
      }
    }

    allConflicts

  }


  lazy val funEquations = {
    (for (pl <- pseudoLiterals) yield {
      pl.funEquations
    }).flatten
  }

  lazy val equations = pseudoLiterals.map(_.lit).filter(_.isPositiveFlatEquation)

  lazy val order = {
    import scala.collection.mutable.ListBuffer
    val tmpOrder = ListBuffer() : ListBuffer[Term]
    for (pl <- pseudoLiterals) {
      for (feq <- pl.funs)
        if (!tmpOrder.contains(feq.res))
          tmpOrder += feq.res
      for (t <- pl.lit.terms)
        if (!tmpOrder.contains(t))        
          tmpOrder += t
    }

    new Order(tmpOrder.toList)
  }
    

  def tryClose() : Option[Branch] = {
    // TODO: This always works
    println("Trying to close...")
    println(this)
    println(conflicts)
    if (conflicts.length == 0) {
      None
    } else {
      // Add BREU call here...

      val funEqs = this.funEquations
      val eqs = this.equations
      // val argGoals : List[List[(ConstantTerm, ConstantTerm)]] = branch.toBREU
      // (argGoals.toList, funEqs ++ eqs)
      println("FunEqs: " + funEqs.mkString(", "))
      println("Eqs: " + eqs.mkString(", "))
      println("Conflicts: ")
      val goals = 
        for (c <- conflicts) yield {
          println("\t" + c)
          c match {
            // TODO: Handle Other conflicts
            case Branch.ComplementaryPair(a1, a2) => {
                (for ((arg1, arg2) <- a1.args zip a2.args) yield {
                  println("\t\t" + ((arg1, arg2)))
                  (arg1, arg2)
                }).toList
            }
          }
        }



      println("Order:" + this.order)

      // BREU arguments
      val breuDomains = this.order.toDomains()

      // TODO: Handle eqs!
      val breuFlatEqs =
        for (feq <- funEqs) yield {
          (feq.fun, feq.args, feq.res)
        }
      val breuGoals = goals
      // val breuNegFunEqs = List()


      println("<<< BREU >>> ")
      println(breuDomains)
      println(breuFlatEqs)
      println(breuGoals)

      val breuSolver = new breu.LazySolver[Term, String](() => (), 60000)
      val breuProblem = breuSolver.createProblem(breuDomains, List(breuGoals), List(breuFlatEqs))
      println(breuProblem)
      val result = breuProblem.solve
      println(result)
      val closedBranch = new Branch(pseudoLiterals, Branch.InvalidEquation(Term("asd", true), Term("dsa", false)))
      Some(closedBranch)
    }
  }

  def extend(pl : PseudoLiteral) =
    new Branch(pl :: pseudoLiterals, closed)

  
}



      // c match {
      //   case ConnectionNegEq(node) => {
      //     (nodes(node)) match {
      //       case (NegEquation(t1, t2)) => {
      //         List((t1, t2))
      //       }
      //       case _ => throw new Exception("ConnectionNegEq is pointing wrong!")
      //     }
      //   }
      //   case ConnectionCompLits(node1, node2) => {
      //     (nodes(node1), nodes(node2)) match {
      //       case (pl : PositiveLiteral, nl : NegativeLiteral) =>
      //         for ((t1, t2) <- pl.args zip nl.args) yield (t1, t2)
      //       case (nl : NegativeLiteral, pl : PositiveLiteral) =>
      //         for ((t1, t2) <- pl.args zip nl.args) yield (t1, t2)
      //     }
      //         // val pred1atom = (pred1.negativeLits ++ pred1.positiveLits).head
      //         // val pred2atom = (pred2.negativeLits ++ pred2.positiveLits).head

      //         // (node1, node2)
      //         // for ((arg1, arg2) <- (pred1atom zip pred2atom).toList) yield {
      //         //-BEGIN-ASSERTION-/////////////////////////////////////////////////////////
      //         // Debug.assertPre(ConnectionProver.AC, arg1.termIterator.size == 1 && arg2.termIterator.size == 1)
      //         //-END-ASSERTION-//////////////////////////////////////////////////////////
      //           // println("\t" + arg1 + "\t?=\t" + arg2)
      //           // println("\t" + arg1.getClass + " \t?=\t" + arg2.getClass)
      //           // (arg1.lastTerm.constants.head, arg2.lastTerm.constants.head)
      //           // }
      //     //   case _ => throw new Exception("ConncetionCompLits is pointing wrong!")
      //     // }
      //   }
      // }
