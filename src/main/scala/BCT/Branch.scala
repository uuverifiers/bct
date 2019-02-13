package bct

/*
 A branch consists of PseudoLiterals
 */

object Branch {
  def apply(pseudoLiteral : PseudoLiteral, strong : Boolean) : Branch = {
    Branch(List(pseudoLiteral), pseudoLiteral.order, strong)
  }

  def apply(pseudoLiterals : List[PseudoLiteral], order : Order, strong : Boolean) : Branch = {
    Branch(pseudoLiterals, order, false, strong)
  }

  class Conflict
  case class ComplementaryPair(a1 : Atom , a2 : Atom) extends Conflict {
    override def toString() = {
      "C[" + (for ((t1, t2) <- a1.args zip a2.args) yield (t1 + " != " + t2)).mkString(", ") + "]"
    }
  }
  case class InvalidEquation(t1 : Term, t2 : Term) extends Conflict {
    override def toString() = {
      "C[" + t1 + " != " + t2 + "]"
    }
  }

  type Equation = (String, List[Term], Term)
  type Goal = List[List[(Term, Term)]]

  def tryClose(
    testBranch : Branch,
    branches : List[Branch],
    partialModel : Model,
    blockingConstraints : BlockingConstraints,
    maxTime : Long) : (Option[(Model, Model, BlockingConstraints)]) = {
    val testProblem = testBranch.toBreu
    val subProblems = branches.map(_.toBreu)
    if ((testProblem :: subProblems) contains None) {
      D.dprintln("tryClose: couldn't create feasible subProblem")
      None
    } else {
      var domains = Domains.Empty
      val breuSubProblems =
        for (sp <- testProblem :: subProblems) yield {
          val (subDomains, subEqs, subGoals) = sp.get
          domains = domains.extend(subDomains)
          (subGoals, subEqs)
        }

      if (Settings.prune_model)
        domains = domains.pruneWithModel(partialModel)

      val relTerms = testBranch.head.terms

      val breuGoals = breuSubProblems.map(_._1)
      val breuEqs = breuSubProblems.map(_._2)
      val breuSolver = new breu.LazySolver[Term, String]()
      val (posBlockingClauses, negBlockingClauses) = blockingConstraints.toBlockingClauses()
      try {

        val breuProblem =
          breuSolver.createProblem(
            domains.domains,
            breuGoals,
            breuEqs,
            posBlockingClauses,
            negBlockingClauses)



        if (Settings.debug)
          println(breuProblem)
        if (Settings.save_breu) {
          D.breuCount += 1
          val filename = "BREU_PROBLEMS/" + D.breuCount + ".breu"
          breuProblem.saveToFile(filename)
          println("Saved to: " + filename)
        }

        Timer.measure("BREU") {
          breuProblem.solve(maxTime) match {
            case breu.Result.SAT => {
              val positiveConstraints =
                for (bc <- breuProblem.positiveBlockingClauses) yield UnificationConstraint(bc)
              val negativeConstraints =
                for (bc <- breuProblem.negativeBlockingClauses) yield DisunificationConstraint(bc)
              val model = breuProblem.getModel
              val newBc = BlockingConstraints(positiveConstraints ++ negativeConstraints)

              // TODO: Hack to remove min term from model
              val tmpModel : Model =
                Model((for (rt <- relTerms) yield (rt -> model(rt))).toMap).removeMin()

              val fullModel = Model(model)
              Some((tmpModel, fullModel, newBc))
            }
            case breu.Result.UNSAT | breu.Result.UNKNOWN => {
              None
            }
          }
        }
      } catch {
        // TODO: Catch ContradictoryException in BREU?
        case e : org.sat4j.specs.TimeoutException => {
          throw new TimeoutException
        }
      }
    }
  }
}


case class Branch(
  pseudoLiterals : List[PseudoLiteral],
  order : Order,
  val isClosed : Boolean,
  val strong : Boolean) extends Iterable[PseudoLiteral] {
  assert(pseudoLiterals.length > 0)
  def length = pseudoLiterals.length
  def depth = pseudoLiterals.length
  def iterator = pseudoLiterals.iterator
  override def toString() =
    pseudoLiterals.mkString("<-") + " || " + order + " || " + conflicts.mkString(" v ")

  def simpleString() =
    pseudoLiterals.mkString("<-") + " || " + order    

  lazy val closed = Branch(pseudoLiterals, order, true, strong)
  lazy val weak = Branch(pseudoLiterals, order, isClosed, false)

  lazy val conflicts : List[Branch.Conflict] = {
    if (strong) {
      // Only allow contradiction involving last one or two nodes
      val n1 = pseudoLiterals(0)

      // Only use this if branch length > 0
      lazy val n2 = pseudoLiterals(1)

      // Three cases:
      // (1) First node is an inequality, then this inequality must be part of the conflict
      // (2) First node is Positive/NegativeLiteral,
      //     second node is an equality, then first node must be in conflict (using equality)
      // (3) First and second node is a Positive/NegativeLiteral, then they must be in conflict
      // (4) First node is an equality, then this equality must be part of the conflict

      if (n1.lit.isNegativeEquation) {
        // Case 1
        val (lhs, rhs) = (n1.equation.lhs, n1.equation.rhs)
        List(Branch.InvalidEquation(lhs, rhs))
      } else if (pseudoLiterals.length >= 2 && n1.lit.isAtom && n2.lit.isPositiveEquation) {
        // Case 2
        // TODO: Add check that n1 and n3 actually uses equation in n2.
        for (n3 <- pseudoLiterals.drop(2); if n1.isComplementary(n3))
        yield Branch.ComplementaryPair(n1.atom, n3.atom)
      } else if (pseudoLiterals.length >= 2 && n1.isComplementary(n2)) {
        // Case 3
        List(Branch.ComplementaryPair(n1.atom, n2.atom))
      } else if (n1.lit.isPositiveEquation) {
        // Case 4
        this.weak.conflicts
      } else {
        List()
      }
    } else {
      val pairConflicts : List[Branch.Conflict]  =
        (for (
          i1 <- 0 until pseudoLiterals.length;
          i2 <- (i1+1) until pseudoLiterals.length;
          if pseudoLiterals(i1).isComplementary(pseudoLiterals(i2)))
        yield Branch.ComplementaryPair(pseudoLiterals(i1).atom, pseudoLiterals(i2).atom)).toList

      val singleConflict : List[Branch.Conflict] =
        for (pl <- pseudoLiterals; if pl.lit.isNegativeEquation) yield {
          val (lhs, rhs) = (pl.equation.lhs, pl.equation.rhs)
          Branch.InvalidEquation(lhs, rhs)
        }

      pairConflicts ++ singleConflict
    }
  }


  lazy val funEquations = {
    pseudoLiterals.map(_.funEquations).flatten
  }

  lazy val equations = pseudoLiterals.map(_.lit).filter(_.isPositiveEquation)

  lazy val toUnification : List[List[(Term, Term)]] = {
    (for (c <- conflicts) yield {
      c match {
        case Branch.ComplementaryPair(a1, a2) => {
          (for ((arg1, arg2) <- a1.args zip a2.args) yield {
            (arg1, arg2)
          }).toList
        }

        case Branch.InvalidEquation(t1, t2) => {
          List((t1, t2))
        }
      }
    }).toList
  }

  lazy val toBreu : Option[(Domains, List[Branch.Equation], Branch.Goal)] = {
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
              List((t1, t2))
            }
          }
        }


      // BREU arguments
      val breuDomains = this.order.toDomains()

      var nextDummyPredicate = 0
      val breuFlatEqs1 =
        (for (PositiveEquation(lhs, rhs) <- eqs) yield {
          nextDummyPredicate += 1
          val newPred = "dummy_predicate_" + nextDummyPredicate
          List((newPred, List(), lhs), (newPred, List(), rhs))
        }).flatten

      val breuFlatEqs2 =
        for (feq <- funEqs) yield {
          (feq.fun, feq.args, feq.res)
        }
      val breuGoals = goals

      Some((breuDomains, breuFlatEqs1 ++ breuFlatEqs2, breuGoals))
    }
  }

  def extend(pl : PseudoLiteral, augOrder : Order) = {
    val newOrder = order + augOrder    
    Branch(pl :: pseudoLiterals, newOrder, isClosed, strong)
  }

  lazy val regularityConstraints : BlockingConstraints = {
    val h = pseudoLiterals.head
    BlockingConstraints(
      for (pl <- pseudoLiterals.tail; if (h.lit.regularityConstraint(pl.lit).isDefined))
      yield h.lit.regularityConstraint(pl.lit).get
    )
  }

  def instantiate(model : Model) = {
    // TODO: Maybe we can change the order here in a clever way?
    val newPseudoLiterals = pseudoLiterals.map(_.instantiate(model))
    Branch(newPseudoLiterals, order, isClosed, strong)
  }

  def tryClose(maxTime : Long) =
    Branch.tryClose(this, List(), Model.Empty, BlockingConstraints.Empty, maxTime)
}
