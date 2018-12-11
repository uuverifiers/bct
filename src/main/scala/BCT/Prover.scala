package bct

// TODO: Insert assertions

object Prover {
  var debug = false
  var startTime : Long = 0
  var timeoutReached = false

  def dprint(str : String) =
    if (debug) print(str)


  def dprintln(str : String) = dprint(str + "\n")
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
    if (step == 0) {
      dprintln("\tclose()")
      table.close()
    } else {
      val (clause, idx) = litIdx(step-1, inputClauses)
      val copiedClause = clause.copy(tableStep.toString)
      dprintln("Extend and close w. " + copiedClause + " idx " + idx)
      table.extendAndClose(copiedClause, idx)
    }
  }


  var PROVE_TABLE_STEP = 0
  def proveTable(table : Table, inputClauses : List[PseudoClause], timeout : Long, step : Int = 0, steps : List[Int] = List())(implicit MAX_DEPTH : Int) : Option[Table] = {
    dprintln("\nProveTable...(" + steps.reverse.mkString(",") + "> " + step + ") .... (" + PROVE_TABLE_STEP +")")
    PROVE_TABLE_STEP += 1
    if (System.currentTimeMillis - startTime > timeout) {
      timeoutReached = true
      None
    } else if (table.isClosed) {
      dprintln("\tClosed!")
      Some(table)
    } else if (table.depth > MAX_DEPTH) {
      dprintln("\tmax depth!")
      None
    } else {
      // We first try to extend the table. Then we loop over different ways of closing it. Two-level loop.
      // If we are at maximum depth, only allow step 0 (i.e., direct closing)
      val maxStep =
        if (table.depth == MAX_DEPTH)
          0
        else
          inputClauses.map(_.length).sum

      // Did we try every step?      
      if (step > maxStep) {
        dprintln("\tmax step (" + maxStep + ")!")
        // BACKTRACK
        None
      } else {
        // Extract open branch:
        val branch = table.nextBranch
        // Let's find a conflict
        handleStep(table, step, branch, inputClauses, PROVE_TABLE_STEP) match {
          case None => proveTable(table, inputClauses, timeout, step + 1, steps)
          case Some(nextTable) => {
            dprintln(nextTable.fullString())
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


  def prove(inputClauses : List[PseudoClause], timeout : Long, debug_ : Boolean = false) = {
    debug = debug_
    println("Proving...")
    var result = None : Option[Table]
    var inputClause = 0

    if (debug) {
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
        println(step + "\t" + str)
      }
    }

    startTime = System.currentTimeMillis
    timeoutReached = false

    Timer.measure("Prove") {
      // We have to try all input clauses
      while (!result.isDefined && inputClause < inputClauses.length) {
        val iClause = inputClauses(inputClause)
        dprintln("<<<Input Clause: " + iClause + ">>>")
        val table = Table(iClause)
        dprintln(table.toString)
        var maxDepth = 3
        while (!result.isDefined && maxDepth < 8) {
          result = proveTable(table, inputClauses, timeout)(maxDepth)
          maxDepth += 1
        }
        inputClause += 1
      }
    }


    dprintln(Timer.toString)
    if (timeoutReached)
      println("TIMEOUT")
    result
  }
}
