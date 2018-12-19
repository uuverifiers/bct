package bct

import scala.collection.mutable.{Map => MMap}

// TODO: Insert assertions

class TimeoutException extends Exception

object Prover {
  var startTime : Long = 0
  var maxTime : Long = 0
  var maxDepthReached = false
  var lastAction = ""

  // Given steps and set of clauses, return which clause and which index to use
  def litIdx(step : Int, inputClauses : List[PseudoClause], clauseIdx : Int = 0) : (PseudoClause, Int) = {
    if (step < inputClauses(clauseIdx).length)
      (inputClauses(clauseIdx), step)
    else
      litIdx(step - inputClauses(clauseIdx).length, inputClauses, clauseIdx+1)
  }

  def litIdxAux(step : Int, inputClauses : List[PseudoClause], clauseIdx : Int = 0) : (Int, Int) = {
    if (step < inputClauses(clauseIdx).length)
      (clauseIdx, step)
    else
      litIdxAux(step - inputClauses(clauseIdx).length, inputClauses, clauseIdx+1)
  }  


  // // Input: One branch and one step
  // // Output: If step is not applicable, None, else a tuple with the branch closed and new open branches
  // def handleStep(table: Table, step : Int, branch : Branch, inputClauses : List[PseudoClause], tableStep : Int) : Option[Table] = {
  //   // Convert step to Closer
  //   val remTime = maxTime - (System.currentTimeMillis - startTime)
  //   if (step == 0) {
  //     lastAction = "\tclose()"
  //     table.close(step, remTime)
  //   } else {
  //     val (clause, idx) = litIdx(step-1, inputClauses)
  //     val copiedClause = clause.copy(tableStep.toString)
  //     lastAction = "Extend and close w. " + copiedClause + " idx " + idx
  //     D.dprintln("Trying: " + lastAction)
  //     table.extendAndClose(copiedClause, idx, step, remTime)
  //   }
  // }

  var PROVE_TABLE_STEP = 0
  def proveTable(table : Table, inputClauses : List[PseudoClause], literalMap : Map[(String, Boolean), List[(Int, Int)]], timeout : Long, steps : List[(Int, Int)] = List())(implicit MAX_DEPTH : Int) : Option[Table] = {
    D.dprintln("proveTable(..., " + steps.mkString(">") + ")")
    if (!steps.isEmpty)
      D.dprintln(steps.reverse.mkString(">"))
    PROVE_TABLE_STEP += 1
    if (System.currentTimeMillis - startTime > timeout) {
      throw new TimeoutException
    } else if (table.isClosed) {
      D.dprintln("\tClosed!")
      Some(table)
    } else if (table.depth > MAX_DEPTH) {
      // D.dprintln("\nProveTable...(" + steps.reverse.mkString(",") + "> " + step + ") .... (" + PROVE_TABLE_STEP +")")
      D.dprintln("\tmax depth!")
      maxDepthReached = true
      None
    } else {
      val allSteps = (for (ic <- inputClauses.indices; idx <- 0 until inputClauses(ic).length) yield { (ic, idx) }).toList
      val possibleSteps =
        table.nextBranch.head.lit match {
          case PositiveLiteral(a) => literalMap((a.predicate, true))
          case NegativeLiteral(a) => literalMap((a.predicate, false))
          case _ => allSteps
        }

      for ((clause, idx) <- ((-1,-1) :: possibleSteps)) {
        val branch = table.nextBranch
        val remTime = maxTime - (System.currentTimeMillis - startTime)

        val handleResult = 
          if (clause == -1) {
            lastAction = "\tclose()"
            table.close((-1,-1), remTime)
          } else {
            val copiedClause = inputClauses(clause).copy(PROVE_TABLE_STEP.toString)
            lastAction = "Extend and close w. " + copiedClause + " idx " + idx
            D.dprintln("Trying: " + lastAction)
            table.extendAndClose(copiedClause, idx, (clause, idx), remTime)
          }

        if (handleResult.isDefined) {
          D.dprintln("\nProveTable...(" + steps.reverse.mkString(",") + "> " + (clause, idx) + ") .... (" + PROVE_TABLE_STEP +")")
          D.dprintln(lastAction)
          D.dprintln(handleResult.get.toString)
          val nextTable = proveTable(handleResult.get, inputClauses, literalMap, timeout, (clause,idx) :: steps)
          if (nextTable.isDefined && nextTable.get.isClosed)
            return nextTable
        }
      }
      None
    }
  }


  def proveaux(inputClauses : List[PseudoClause], timeout : Long, forcedStartClause : Option[Int]) = {
    D.dprintln("Proving...")
    var result = None : Option[Table]
    var startClause = 0


    if (D.debug) {
      val maxStep = inputClauses.map(_.length).sum
      for (step <- 0 to maxStep) {
        val str =
          if (step == 0) {
            "(-1, -1) close()"
          } else {
            val (clause, idx) = litIdx(step-1, inputClauses)
            val (clauseInt, idxInt) = litIdxAux(step-1, inputClauses)
            val copiedClause = clause.copy("...")
            "(" + clauseInt + ", " + idxInt + ") Extend and close w. " + copiedClause + " idx " + idx
          }
        D.dprintln(step + "\t" + str)
      }
    }

    maxTime = timeout
    startTime = System.currentTimeMillis

    // TODO: Begin by trying to close the unit-clause tableaux (with weak connections I guess)
    val unitClauses = inputClauses.filter(_.length == 1).map(_.head)

    D.dprintln("UnitClauses:")
    for (uc <- unitClauses)
      D.dprintln("\t" + uc)


    // Lets build up a structure which contains all clauses with a specific literal (and negation)
    // Map[Predicate, Negated] => List((Int, Int))

    val literalMap = MMap() : MMap[(String, Boolean), List[(Int, Int)]]

    for ((ic, i) <- inputClauses.zipWithIndex) {
      for ((pl, j) <- ic.zipWithIndex) {
        pl.lit match {
          case PositiveLiteral(a) => {
            val key = (a.predicate, false)
            val index = (i, j)
            literalMap += key -> (index :: literalMap.getOrElse(key, List()))
          }
          case NegativeLiteral(a) => {
            val key = (a.predicate, true)
            val index = (i, j)
            literalMap += key -> (index :: literalMap.getOrElse(key, List()))            
          }

            // TODO: What to do with these?
          case PositiveEquation(lhs, rhs) => {
          }
          case NegativeEquation(lhs, rhs) => {
          }            
        }
      }
    }

    for ((k, v) <- literalMap) {
      D.dprintln(k + " -> " + v.mkString(", "))
    }

    val candidateStartClauses = inputClauses.filter(_.length > 1).toList

    val startClauses : List[PseudoClause] =
      if (forcedStartClause.isDefined)
        List(candidateStartClauses(forcedStartClause.get))
      else if (candidateStartClauses.isEmpty)
        List(inputClauses.head)
      else
        candidateStartClauses

    Timer.measure("Prove") {
      // We have to try all input clauses
      while (!result.isDefined && startClause < startClauses.length) {

        // We need to start with all unit clauses
        val iClause = startClauses(startClause)
        D.dprintln("<<<Start Clause (" + startClause + "): " + iClause + ">>>")
        val table = Table.create(iClause, unitClauses)
        var searchCompleted = false
        maxDepthReached = false
        D.dprintln(table.toString)
        var maxDepth = 3
        while (!result.isDefined && !searchCompleted) {
          result = proveTable(table, inputClauses, literalMap.toMap, timeout)(maxDepth)
          if (maxDepthReached) {
            D.dprintln("Increasing max depth: " + maxDepth)
            maxDepth += 1
          } else {
            searchCompleted = true
          }
        }
        startClause += 1
      }
    }

    result
  }

  def prove(inputClauses : List[PseudoClause], timeout : Long, startClause : Option[Int] = None) = {
    val result = proveaux(inputClauses, timeout, startClause)
    // if (result.isDefined) {
    //   val table = result.get
    //   val model = table.fullModel
    //   val patchedModel = Model(
    //     for ((t, v) <- model.assignments) yield {
    //       if (t.isUniversal && t == v)
    //         t -> Order.MIN_TERM
    //       else
    //         t -> v
    //     })
    // }
    result
  }
}
