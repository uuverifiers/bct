package bct

object Prover {
  // Currently dummy. Int gives the number of input clauses.
  var allCount = -1
  var exCount = -1

  def newEx() = {
    exCount += 1
    "EX_" + exCount
  }

  def newAll() = {
    allCount += 1
    "ALL_" + allCount
  }  

  def getInputClause(i : Int) = {
    val pred = "R"
    val fun = "f"

    if (i == 0) { // R(x) v R(f(x))
      val x = newAll()
      val a1 = Atom(pred, List(x)) // R(x)

      val c1 = newEx()
      val feq1 = FunEquation(fun, List(x), c1)
      val a2 = Atom(pred, List(c1))

      val l1 = PseudoLiteral(PositiveLiteral(a1))
      val l2 = PseudoLiteral(List(feq1), PositiveLiteral(a2))
      PseudoClause(List(l1, l2))
    } else { // !R(x) v !R(f(f(x)))
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
    }
  }


  def proveTable(table : Table, clauses : Int, nextStep : Int = 0) : Option[Table] = {
    println("ProveTable...")
    println(table)
    if (table.isClosed) {
      Some(table)
    } else {
      // Did we try every step?
      val maxStep = clauses.map(_.length).sum
      if (nextStep == maxStep) {
        None
      } else {
        // Extract open branch:
        val branch = table.nextBranch

        // Let's find a conflict

        val conflict : Branch.Conflict = {
          Branch.ComplementaryPair(0,1)
        }

        // If one is found, close table otherwise we have to backtrack
        conflict match {
          case Branch.Open => {
            // BACKTRACK
          }
          case conflict => {
            val nextTable = table.close(conflict)
            proveTable(nextTable, clauses)
          }
        }
      }
    }
  }

  def prove(clauses : Int) = {
    println("Proving...")
    val table = Table(getInputClause(0))
    println(table)
    val result = proveTable(table, 2)
    println(result)

  }
}
