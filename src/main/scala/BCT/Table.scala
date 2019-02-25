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

//
//  Represents a proof table
//

class Table(
  openBranches : List[Branch],
  closedBranches : List[Branch],
  val steps : List[(Int, Int)] = List(),
  val partialModel : Model = Model.Empty,
  blockingConstraints : BlockingConstraints = BlockingConstraints.Empty)
  (implicit strong : Boolean = true) {



  val length = openBranches.length + closedBranches.length
  val depth = if (openBranches.isEmpty) 0 else openBranches.map(_.depth).max
  val isClosed = openBranches.length == 0
  lazy val nextBranch = openBranches.head



  override def toString =
    "\ttable\n" + 
    (if (!openBranches.isEmpty)
      "----open----\n" + openBranches.mkString("\n") + "\n"
    else
      "") +
    (if (!closedBranches.isEmpty)
      "---closed---\n" + closedBranches.mkString("\n") + "\n"
    else
      "") + "\n" +
      "steps: " + steps.reverse.mkString(".") + "\n" +
      "Model: \n" + partialModel

  val simple =
    "\ttable\n" + 
    (if (!openBranches.isEmpty)
      "----open----\n" + openBranches.mkString("\n") + "\n"
    else
      "")


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
    // TODO: We need to add regularity constraints
    val regularityConstraints : BlockingConstraints =
      (tBranch :: rBranches).map(_.regularityConstraints).fold(BlockingConstraints.Empty)(_ ++ _)

    // If we are instantiating, we do not need to check the closed branches
    val actualClosedBranches =
      if (Settings.instantiate)
        List()
      else
        closedBranches

    tryClose(
      tBranch,
      partialModel,
      // blockingConstraints ++ regularityConstraints,
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
    branch : Branch,
    partialModel : Model,
    maxTime : Long) : (Option[(Model, Model, BlockingConstraints)]) = Timer.measure("BREU-new") {
    val breuSolver = Prover.breuSolver

    // println(branch)
    val relTerms = branch.head.terms

    if (branch.conflicts.length == 0) {
      return None
    } else {
      val funEqs = branch.funEquations
      val eqs = branch.equations
      val goals =
        for (c <- branch.conflicts) yield {
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
      for (t <- branch.order.termList) {
        tmpTerms += t
        if (!(breuSolver.getAddedTerms() contains t)) {
          val d =
            if (partialModel contains t) {
              Set(partialModel(t))
            } else if(t.isUniversal) {
              tmpTerms.toSet
            } else {
              Set(t)
            }
          breuSolver.addVariable(t, d)
        }
      }

      val breuFlatEqs1 =
        (for (PositiveEquation(lhs, rhs) <- eqs) yield {
          val newPred = breuSolver.genDummyPredicate()
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


    try {

      if (Settings.debug)
        println(breuSolver)

      val result = 
        Timer.measure("BREU-solve") {
        breuSolver.solve(maxTime)
      }
      result match {
        case breu.Result.SAT => {
          val model = breuSolver.getModel()

          // TODO: Hack to remove min term from model          
          val tmpModel : Model =
            Model((for (rt <- relTerms) yield (rt -> model(rt))).toMap).removeMin()

          val (posBC, negBC) = breuSolver.getBlockingClauses()
          val positiveConstraints =
            for (bc <- posBC) yield UnificationConstraint(bc)
          val negativeConstraints =
            for (bc <- negBC) yield DisunificationConstraint(bc)


          val newBc = BlockingConstraints(positiveConstraints ++ negativeConstraints)

          Some((tmpModel, Model(model), newBc))
        }
        case breu.Result.UNSAT | breu.Result.UNKNOWN => {
          breuSolver.pop()
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
