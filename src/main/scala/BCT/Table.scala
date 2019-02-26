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
  val regularityConstraints : List[DisunificationConstraint] = List())
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
      "Model: \n" + (if (Settings.instantiate) partialModel else "n/a")

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
    val newRegularityConstraints : List[DisunificationConstraint] =
      if (Settings.regularity)
        (tBranch :: rBranches).map(_.regularityConstraints).flatten
      else
        List()

    val allRegularityConstraints = newRegularityConstraints ++ regularityConstraints

    // If we are instantiating, we do not need to check the closed branches
    val actualClosedBranches =
      if (Settings.instantiate)
        List()
      else
        closedBranches

    tryClose(
      tBranch,
      partialModel,
      List() : List[UnificationConstraint],
      allRegularityConstraints : List[DisunificationConstraint],
      maxTime) match {
      case None => None
      case Some((relModel, newFullModel)) => {
        // Only do partial model extension if we are instantiating...
        val newPartialModel = if (Settings.instantiate) partialModel.extend(relModel).get else partialModel
        val closedTable =
          new Table(
            rBranches ++ openBranches.tail,
            tBranch.closed :: closedBranches,
            step :: steps,
            newPartialModel,
            allRegularityConstraints)
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
    val newRegularityConstraints = regularityConstraints.map(_.instantiate(model))
    new Table(
      newOpenBranches,
      newClosedBranches,
      steps,
      partialModel,
      newRegularityConstraints)(strong)
  }

  def tryClose(
    branch : Branch,
    partialModel : Model,
    unificationConstraints : List[UnificationConstraint],
    disunificationConstraints : List[DisunificationConstraint],
    maxTime : Long) : (Option[(Model, Model)]) = Timer.measure("BREU-new") {
    val breuSolver = Prover.breuSolver

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
            if (Settings.instantiate && (partialModel contains t)) {
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

      if (!unificationConstraints.isEmpty)
        throw new Exception("Unification Constraints provided!")

      for (du <- disunificationConstraints) {
        breuSolver.addDisunificationConstraint(du.c)
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

          // TODO: Make relevant term extraction happen in caller instead of here.
          val tmpModel : Model =
            Model((for (rt <- relTerms) yield (rt -> model(rt))).toMap)

          Some((tmpModel, Model(model)))
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
