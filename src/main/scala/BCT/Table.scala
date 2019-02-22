package bct

import scala.collection.mutable.{Set => MSet}

/*
 A table consists of Branches

 When we extend by using PseudoClause, we split it into PseudoLiterals (one for each branch)

 */

object Table {

  // Constructors
  def apply(branches : List[Branch]) = {
    new Table(branches, List())
  }

  def apply(cls : PseudoClause, strong : Boolean = true) = {
    create(cls, List(), strong)
  }


  // Special constructor for creating with unit clauses
  def create(cls : PseudoClause, unitClauses : List[PseudoLiteral], strong : Boolean = true) = {
    val branches = (for (l <- cls.toPseudoLiterals()) yield {
      val branchOrder = 
        if (unitClauses.isEmpty) {
          cls.order
        } else {
          val revUnitClauses = unitClauses.reverse
          var tmpOrder = revUnitClauses.head.order
          for (uc <- revUnitClauses.tail)
            tmpOrder = tmpOrder + uc.order
          tmpOrder + cls.order
        }
      Branch(l :: unitClauses, branchOrder, strong)
    }).toList
    new Table(branches, List())(strong)
  }
}

class Table(
  openBranches : List[Branch],
  closedBranches : List[Branch],
  val steps : List[(Int, Int)] = List(),
  val partialModel : Model = Model.Empty,
  blockingConstraints : BlockingConstraints = BlockingConstraints.Empty)
  (implicit strong : Boolean = true) {

  override def toString =
    "\ttable\n" + 
    (if (!openBranches.isEmpty)
      "----open----\n" + openBranches.mkString("\n") + "\n"
    else
      "") +
    (if (!closedBranches.isEmpty)
      "---closed---\n" + closedBranches.mkString("\n") + "\n"
    else
      "") + "\n"
  // "steps: " + steps.reverse.mkString(".") + "\n" +
  // "Model: \n" + partialModel

  val simple =
    "\ttable\n" + 
    (if (!openBranches.isEmpty)
      "----open----\n" + openBranches.mkString("\n") + "\n"
    else
      "")
  // "steps: " + steps.reverse.mkString(".") + "\n" +
  // "Model: \n" + partialModel  

  

  val length = openBranches.length + closedBranches.length
  val depth = if (openBranches.isEmpty) 0 else openBranches.map(_.depth).max
  val isClosed = openBranches.length == 0
  lazy val nextBranch = openBranches.head

  def close(maxTime : Long) : Option[Table] =
    extendAndClose(PseudoClause.Empty, 0, (-1, -1), maxTime)

  def extendAndClose(
    clause : PseudoClause,
    idx : Int,
    step : (Int, Int),
    maxTime : Long) : Option[Table] = {
    val (tBranch, rBranches) =
      if (clause.isEmpty) {
        (nextBranch.weak, List())
      } else {
        val branch = nextBranch
        val newBranches =
          (for (pl <- clause.toPseudoLiterals()) yield branch.extend(pl, clause.order)).toList
        val testBranch = newBranches(idx)
        val restBranches = newBranches.take(idx) ++ newBranches.drop(idx+1)
        (testBranch, restBranches)
      }

    // Extract regularity constraints
    // I.e., if the newly added head of a branch is similar in structure to a previous,
    // at least one of the literals must differ (i.e. a negative blocking clause)
    val regularityConstraints : BlockingConstraints =
      (tBranch :: rBranches).map(_.regularityConstraints).fold(BlockingConstraints.Empty)(_ ++ _)

    // If we are instantiating, we do not need to check the closed branches
    // val actualClosedBranches =
    //   if (Settings.instantiate)
    //     List()
    //   else
    //     closedBranches

    tryClose(
      tBranch,
      partialModel,
      blockingConstraints ++ regularityConstraints,
      maxTime) match {
      case None => None
      case Some((relModel, newFullModel, blockingConstraints)) => {
        // TODO: What if model extension doesn't work
        val newPartialModel = partialModel.extend(relModel).get        
        val closedTable =
          new Table(
            rBranches ++ openBranches.tail,
            tBranch.closed :: closedBranches,
            step :: steps,
            newPartialModel,
            blockingConstraints)
        if (Settings.instantiate)
          Some(closedTable.instantiate(newPartialModel))
        else
          Some(closedTable)
      }
    }
  }


  def instantiate(model : Model) = {
    val newOpenBranches = openBranches.map(_.instantiate(model))
    val newClosedBranches = closedBranches.map(_.instantiate(model))
    val newBlockingConstraints = blockingConstraints.instantiate(model)
    new Table(
      newOpenBranches,
      newClosedBranches,
      steps,
      partialModel,
      newBlockingConstraints)(strong)
  }

  def tryClose(
    testBranch : Branch,
    partialModel : Model,
    blockingConstraints : BlockingConstraints,
    maxTime : Long) : (Option[(Model, Model, BlockingConstraints)]) = {

    val branches = closedBranches

    Settings.breu match {
      case "old" => tryCloseOldBreu(testBranch, branches, partialModel, blockingConstraints, maxTime)
      case "new" => tryCloseNewBreu(testBranch, branches, partialModel, blockingConstraints, maxTime)
      case "both" => {
        val ret1 = tryCloseOldBreu(testBranch, branches, partialModel, blockingConstraints, maxTime)
        val ret2 = tryCloseNewBreu(testBranch, branches, partialModel, blockingConstraints, maxTime)
        if (Settings.debug) {
          println("ret1:  " + ret1)
          println("ret2: " + ret2)
        }
        assert(ret1.isDefined == ret2.isDefined)
        ret1
      }
    }
  }

  def tryCloseOldBreu(
    testBranch : Branch,
    branches : List[Branch],
    partialModel : Model,
    blockingConstraints : BlockingConstraints,
    maxTime : Long) : (Option[(Model, Model, BlockingConstraints)]) = Timer.measure("BREU-old") {

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

      // if (Settings.prune_model)
      //   domains = domains.pruneWithModel(partialModel)

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


        println("BLOCKING CONSTRAINTS")
        println("pos")
        println(posBlockingClauses.mkString("..."))
        println("neg")
        println(negBlockingClauses.mkString("..."))

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

            Some((tmpModel, Model(model), newBc))
          }
          case breu.Result.UNSAT | breu.Result.UNKNOWN => {
            None
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


  def tryCloseNewBreu(
    testBranch : Branch,
    branches : List[Branch],    
    partialModel : Model,
    blockingConstraints : BlockingConstraints,
    maxTime : Long) : (Option[(Model, Model, BlockingConstraints)]) = Timer.measure("BREU-new") {

    val breuSolver = Prover.breuSolver

    breuSolver.restart()

    val relTerms = testBranch.head.terms
    val addedTerms = MSet() : MSet[Term]

    val (posBlockingClauses, negBlockingClauses) = blockingConstraints.toBlockingClauses()
    try { 
      for (p <- posBlockingClauses)
        breuSolver.addUnificationConstraint(p)

      for (p <- negBlockingClauses)
        breuSolver.addDisunificationConstraint(p)
    } catch {
      case e : org.sat4j.specs.ContradictionException => {
        return None
      }
    }

    for (b <- testBranch :: branches) {
      if (b.conflicts.length == 0) {
        return None
      } else {
        val funEqs = b.funEquations
        val eqs = b.equations
        val goals =
          for (c <- b.conflicts) yield {
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
        val tmpTerms = MSet() : MSet[Term]
        for (t <- b.order.termList) {
          tmpTerms += t
          if (!(addedTerms contains t)) {
            val d =
              if (t.isUniversal)
                tmpTerms.toSet
              else
                Set(t)
            breuSolver.addDomain(t, d)
            addedTerms += t
          }
        }

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

        for (eq <- breuFlatEqs1 ++ breuFlatEqs2) {
          breuSolver.addFunction(eq)
        }


        val breuGoals = goals

        for (g <- goals) {
          breuSolver.addGoal(g)
        }



        breuSolver.push()
      }
    }


    try {

      if (Settings.debug)
        println(breuSolver)
      // if (Settings.save_breu) {
      //   D.breuCount += 1
      //   val filename = "BREU_PROBLEMS/" + D.breuCount + ".breu"
      //   breuProblem.saveToFile(filename)
      //   println("Saved to: " + filename)
      // }

      // println("NEW BREU PROBLEM")
      // println(breuSolver.toString())

      val result = breuSolver.solve(maxTime)
      result match {
        case breu.Result.SAT => {
          val (posBC, negBC) = breuSolver.getBlockingClauses()
          val positiveConstraints =
            for (bc <- posBC) yield UnificationConstraint(bc)
          val negativeConstraints =
            for (bc <- negBC) yield DisunificationConstraint(bc)

          val newBc = BlockingConstraints(positiveConstraints ++ negativeConstraints)

          // TODO: Hack to remove min term from model
          val model = breuSolver.getModel()
          val tmpModel : Model =
            Model((for (rt <- relTerms) yield (rt -> model(rt))).toMap).removeMin()

          Some((tmpModel, Model(model), newBc))
        }
        case breu.Result.UNSAT | breu.Result.UNKNOWN => {
          None
        }
      }
    } catch {
      case e : org.sat4j.specs.ContradictionException => {
        return None
      }      
      case e : org.sat4j.specs.TimeoutException => {
        throw new TimeoutException
      }
    }
  }
}
