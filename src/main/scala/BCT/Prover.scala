package bct

// TODO: Make sure to use lazy val when possible

object Prover {
  // Currently dummy. Int gives the number of input clauses.
  var allCount = -1
  var exCount = -1

  def newEx() = {
    exCount += 1
    Term("c_" + exCount, false)
  }

  def newAll() = {
    allCount += 1
    Term("x_" + allCount, true)
  }  

  def getInputClause(i : Int) = {
    val pred = "R"
    val fun = "f"

    if (i == -1) { // R(x)
      val x = newAll()
      val a1 = Atom(pred, List(x)) // R(x)

      val l1 = PseudoLiteral(PositiveLiteral(a1))
      PseudoClause(List(l1))
    } else if (i == 0) { // R(x) v R(f(x))
      val x = newAll()
      val a1 = Atom(pred, List(x)) // R(x)

      val c1 = newEx()
      val feq1 = FunEquation(fun, List(x), c1)
      val a2 = Atom(pred, List(c1))

      val l1 = PseudoLiteral(PositiveLiteral(a1))
      val l2 = PseudoLiteral(List(feq1), PositiveLiteral(a2))
      PseudoClause(List(l1, l2))
    } else if (i == 1) { // !R(x) v !R(f(f(x)))
      val x = newAll()
      val a1 = Atom(pred, List(x)) // R(x)

      val c1 = newEx()
      val feq1 = FunEquation(fun, List(x), c1)
      val a2 = Atom(pred, List(c1))

      val c2 = newEx()
      val feq2 = FunEquation(fun, List(c1), c2)
      val a3 = Atom(pred, List(c2))

      val l1 = PseudoLiteral(NegativeLiteral(a1))
      val l2 = PseudoLiteral(List(feq1, feq2), NegativeLiteral(a3))
      PseudoClause(List(l1, l2))
    } else {
      throw new Exception("Underfined Input Clause")
    }
  }

  // Given steps and set of clauses, return which clause and which index to use
  def litIdx(step : Int, clauses : Int, clauseIdx : Int = 0) : (PseudoClause, Int) = {
    if (step < getInputClause(clauseIdx).length)
      (getInputClause(clauseIdx), step)
    else
      litIdx(step - getInputClause(clauseIdx).length, clauses, clauseIdx+1)
  }



  // Input: One branch and one step
  // Output: If step is not applicable, None, else a tuple with the branch closed and new open branches
  def handleStep(table: Table, step : Int, branch : Branch, clauses : Int) : Option[Table] = {
    // Convert step to Closer
    if (step == 0) {
      table.close()
    } else {
      val (clause, idx) = litIdx(step-1, clauses)
      table.extendAndClose(clause, idx)
    }
  }


  var PROVE_TABLE_STEP = 0
  def proveTable(table : Table, clauses : Int, step : Int = 0, steps : List[Int] = List())(implicit MAX_DEPTH : Int) : Option[Table] = {
    println("\nProveTable...(" + steps.reverse.mkString(",") + "> " + step + ") .... (" + PROVE_TABLE_STEP +")")
    PROVE_TABLE_STEP += 1
    if (table.isClosed) {
      println("\tClosed!")
      Some(table)
    } else if (table.depth > MAX_DEPTH) {
      println("\tmax width!")
      None
    } else {
      // Did we try every step?
      // We first try to extend the table. Then we loop over different ways of closing it. Two-level loop.
      val maxStep = (for (i <- 0 until clauses) yield getInputClause(i).length).sum
      if (step > maxStep) {
        println("\tmax step!")
        // BACKTRACK
        None
      } else {
        // Extract open branch:
        val branch = table.nextBranch

        // Let's find a conflict
        handleStep(table, step, branch, clauses) match {
          case None => proveTable(table, clauses, step + 1, steps)
          case Some(nextTable) => {
            println(nextTable.fullString())
            proveTable(nextTable, clauses, 0, step::steps) match {
              case None => proveTable(table, clauses, step + 1,steps)
              case closedTable => closedTable
            }
          }
        }
      }
    }
  }

  def prove(clauses : Int) = {
    println("Proving...")    
    val table = Table(getInputClause(0))
    println(table)
    println("\n\n\n\n")


    var result = None : Option[Table]
    var maxDepth = 1
    while (!result.isDefined && maxDepth < 8) {
      result = proveTable(table, 2)(maxDepth)
      maxDepth += 1
    }

    result match {
      case None => println("No proof found...")
      case Some(closedTable) => println("Proof found:\n" + closedTable.fullString())
    }

  }
}
