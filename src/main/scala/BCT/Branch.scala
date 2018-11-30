package bct

object Branch {
  def apply(pseudoLiterals : List[PseudoLiteral], isClosed : Boolean = false, strong : Boolean = true) = {
    new Branch(pseudoLiterals, isClosed, strong)
  }

  class Conflict
  case class ComplementaryPair(a1 : Atom , a2 : Atom) extends Conflict
  case class InvalidEquation(t1 : Term, t2 : Term) extends Conflict

  type FlatEquation = (String, List[Term], Term)
  type Goal = List[List[(Term, Term)]]

  def tryClose(branches : List[Branch], debug : Boolean = false) : (Option[Model]) = {
    val subProblems = branches.map(_.toBreu)
    if (subProblems contains None) {
      None
    } else {
      var domains = Domains.EmptyDomains
      val breuSubProblems = 
      for (sp <- subProblems) yield {
        val (subDomains, subEqs, subGoals) = sp.get
        domains = domains.extend(subDomains)
        (subGoals, subEqs)
      }

      val breuGoals = breuSubProblems.map(_._1)
      val breuEqs = breuSubProblems.map(_._2)

      val breuSolver = new breu.LazySolver[Term, String](() => (), 60000)
      val breuProblem = breuSolver.createProblem(domains.domains, breuGoals, breuEqs)

      if (debug) {
        println(branches.mkString("\n"))
        println(breuProblem)
      }
      // println(breuProblem)
      val result = breuProblem.solve
      result match {
        case breu.Result.SAT => {
          println("\tSAT")
          Some(Model(breuProblem.getModel))
        }
        case breu.Result.UNSAT | breu.Result.UNKNOWN => {
          println("\tUNSAT or UNKNOWN")
          None
        }
      }
    }
  }
}


object BranchOrdering extends Ordering[Branch] {
  def compare(x : Branch, y : Branch) = {
    x.length.compare(y.length)
  }
}



class Branch(pseudoLiterals : List[PseudoLiteral], val isClosed : Boolean = false, strong : Boolean = true) {
  assert(pseudoLiterals.length > 0)
  def length = pseudoLiterals.length
  def depth = pseudoLiterals.length

  override def toString() = pseudoLiterals.mkString("<-") + " || " + conflicts

  lazy val closed = Branch(pseudoLiterals, true, strong)
  lazy val weak = Branch(pseudoLiterals, isClosed, false)

  lazy val conflicts = {
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
      val n1 = pseudoLiterals(0)
      val pairConflicts : List[Branch.Conflict]  =
        for (n2 <- pseudoLiterals.tail; if n1.isComplementary(n2))
        yield Branch.ComplementaryPair(n1.atom, n2.atom)

      val singleConflict : List[Branch.Conflict] =
        if (n1.lit.isNegativeFlatEquation) {
          val (lhs, rhs) = n1.terms
          List(Branch.InvalidEquation(lhs, rhs))
        } else {
          List()
        }

      pairConflicts ++ singleConflict
    }
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

    def add(t : Term) = {
      if (!tmpOrder.contains(t))
        tmpOrder += t
    }
    
    for (pl <- pseudoLiterals.reverse) {
      for (feq <- pl.funs) {
        for (a <- feq.args)
          add(a)
        add(feq.res)
      }

      for (t <- pl.lit.terms)
        add(t)
    }

    new Order(tmpOrder.toList)
  }

  lazy val toBreu : Option[(Domains, List[Branch.FlatEquation], Branch.Goal)] = {
    if (conflicts.length == 0) {
      None
    } else {
      val funEqs = this.funEquations
      val eqs = this.equations
      val goals =
        for (c <- conflicts) yield {
          c match {
            case Branch.ComplementaryPair(a1, a2) => {
              (for ((arg1, arg2) <- a1.args zip a2.args) yield {
                (arg1, arg2)
              }).toList
            }
            case Branch.InvalidEquation(t1, t2) => {
              // TODO: Handle Invalid Equation
              throw new Exception("Invalid Equations not handled: " + (t1, t2))
            }
          }
        }


      // BREU arguments
      val breuDomains = this.order.toDomains()

      // TODO: Handle eqs!
      val breuFlatEqs =
        for (feq <- funEqs) yield {
          (feq.fun, feq.args, feq.res)
        }
      val breuGoals = goals
      // val breuNegFunEqs = List()

      Some((breuDomains, breuFlatEqs, breuGoals))
    }
  }

  def tryClose() = Branch.tryClose(List(this))

  def extend(pl : PseudoLiteral) = {
    assert(!isClosed)
    Branch(pl :: pseudoLiterals, isClosed, strong)
  }

  
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
