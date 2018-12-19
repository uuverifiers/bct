package bct

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


  // Input: One branch and one step
  // Output: If step is not applicable, None, else a tuple with the branch closed and new open branches
  def handleStep(table: Table, step : Int, branch : Branch, inputClauses : List[PseudoClause], tableStep : Int) : Option[Table] = {
    // Convert step to Closer
    val remTime = maxTime - (System.currentTimeMillis - startTime)
    if (step == 0) {
      lastAction = "\tclose()"
      table.close(step, remTime)
    } else {
      val (clause, idx) = litIdx(step-1, inputClauses)
      val copiedClause = clause.copy(tableStep.toString)
      lastAction = "Extend and close w. " + copiedClause + " idx " + idx
      D.dprintln("Trying: " + lastAction)
      table.extendAndClose(copiedClause, idx, step, remTime)
    }
  }


  var PROVE_TABLE_STEP = 0
  def proveTable(table : Table, inputClauses : List[PseudoClause], timeout : Long, step : Int = 0, steps : List[Int] = List())(implicit MAX_DEPTH : Int) : Option[Table] = {
    D.dprintln("proveTable(..., " + step + ", " + steps.mkString(">") + ")")
    // if (!steps.isEmpty)
    //   println(steps.reverse.mkString(">"))
    PROVE_TABLE_STEP += 1
    if (System.currentTimeMillis - startTime > timeout) {
      throw new TimeoutException
    } else if (table.isClosed) {
      D.dprintln("\tClosed!")
      Some(table)
    } else if (table.depth > MAX_DEPTH) {
    D.dprintln("\nProveTable...(" + steps.reverse.mkString(",") + "> " + step + ") .... (" + PROVE_TABLE_STEP +")")      
      D.dprintln("\tmax depth!")
      maxDepthReached = true
      None
    } else {
      // We first try to extend the table. Then we loop over different ways of closing it. Two-level loop.
      // If we are at maximum depth, only allow step 0 (i.e., direct closing)
      val maxStep =
        // if (table.depth == MAX_DEPTH) {
        //   println("\tlast depth - only step 0")
        //   0
        // } else {
          inputClauses.map(_.length).sum
        // }

      // Did we try every step?      
      if (step > maxStep) {
        // BACKTRACK
        D.dprintln("\tmax step")
        None
      } else {
        // Extract open branch:
        val branch = table.nextBranch
        // Let's find a conflict
        handleStep(table, step, branch, inputClauses, PROVE_TABLE_STEP) match {
          case None => proveTable(table, inputClauses, timeout, step + 1, steps)
          case Some(nextTable) => {
            D.dprintln("\nProveTable...(" + steps.reverse.mkString(",") + "> " + step + ") .... (" + PROVE_TABLE_STEP +")")
            D.dprintln(lastAction)
            D.dprintln(nextTable.toString)
            proveTable(nextTable, inputClauses, timeout, 0, step::steps) match {
              case None => proveTable(table, inputClauses, timeout, step + 1, steps)
              case closedTable => {
                closedTable
              }
            }
          }
        }
      }
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
            "close()"
          } else {
            val (clause, idx) = litIdx(step-1, inputClauses)
            val copiedClause = clause.copy("...")
            "Extend and close w. " + copiedClause + " idx " + idx
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


    // TODO: This is for PUZ001+1.p
    // val startClauses = List(inputClauses(5))// .filter(_.length > 1)
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
          result = proveTable(table, inputClauses, timeout)(maxDepth)
          if (maxDepthReached) {
            println("Increasing max depth: " + maxDepth)
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
