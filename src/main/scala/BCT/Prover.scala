package bct

// TODO: Insert assertions

object Prover {

  // Given steps and set of clauses, return which clause and which index to use
  def litIdx(step : Int, ex : Example, clauseIdx : Int = 0) : (PseudoClause, Int) = {
    if (step < ex.getInputClause(clauseIdx).length)
      (ex.getInputClause(clauseIdx), step)
    else
      litIdx(step - ex.getInputClause(clauseIdx).length, ex, clauseIdx+1)
  }



  // Input: One branch and one step
  // Output: If step is not applicable, None, else a tuple with the branch closed and new open branches
  def handleStep(table: Table, step : Int, branch : Branch, ex : Example) : Option[Table] = {
    // Convert step to Closer
    if (step == 0) {
      table.close()
    } else {
      val (clause, idx) = litIdx(step-1, ex)
      println("Extend and close w. " + clause + " idx " + idx)
      table.extendAndClose(clause, idx)
    }
  }


  var PROVE_TABLE_STEP = 0
  def proveTable(table : Table, ex : Example, step : Int = 0, steps : List[Int] = List())(implicit MAX_DEPTH : Int) : Option[Table] = {
    println("\nProveTable...(" + steps.reverse.mkString(",") + "> " + step + ") .... (" + PROVE_TABLE_STEP +")")
    PROVE_TABLE_STEP += 1
    if (table.isClosed) {
      println("\tClosed!")
      Some(table)
    } else if (table.depth > MAX_DEPTH) {
      println("\tmax depth!")
      None
    } else {
      // We first try to extend the table. Then we loop over different ways of closing it. Two-level loop.
      // If we are at maximum depth, only allow step 0 (i.e., direct closing)
      val maxStep =
        if (table.depth == MAX_DEPTH)
          0
        else
          (for (i <- 0 until ex.clauses) yield ex.getInputClause(i).length).sum

      // Did we try every step?      
      if (step > maxStep) {
        println("\tmax step!")
        // BACKTRACK
        None
      } else {
        // Extract open branch:
        val branch = table.nextBranch
        // Let's find a conflict
        handleStep(table, step, branch, ex) match {
          case None => proveTable(table, ex, step + 1, steps)
          case Some(nextTable) => {
            println(nextTable.fullString())
            proveTable(nextTable, ex, 0, step::steps) match {
              case None => proveTable(table, ex, step + 1,steps)
              case closedTable => {
                closedTable
              }
            }
          }
        }
      }
    }
  }


  def prove(ex : Example) = {
    println("Proving...")
    var result = None : Option[Table]
    var inputClause = 0

    Timer.measure("Prove") {
      // We have to try all input clauses
      while (!result.isDefined && inputClause < ex.clauses) {
        val iClause = ex.getInputClause(inputClause)
        println("<<<Input Clause: " + iClause + ">>>")
        val table = Table(iClause)
        println(table)
        var maxDepth = 1
        while (!result.isDefined && maxDepth < 8) {
          result = proveTable(table, ex)(maxDepth)
          maxDepth += 1
        }
        inputClause += 1
      }
    }

    println(">>>")
    println(Timer)
    // result match {
    //   case None => println("No proof found...")
    //   case Some(closedTable) => println("Proof found:\n" + closedTable.fullString())
    // }
    result
  }
}
