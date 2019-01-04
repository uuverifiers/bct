package bct

import scala.collection.mutable.{Map => MMap}

class TimeoutException extends Exception

object Prover {
  var startTime : Long = 0
  var maxDepthReached = false
  var PROVE_TABLE_STEP = 0

  def proveTable(
    table : Table,
    inputClauses : List[PseudoClause],
    literalMap : Map[(String, Boolean), List[(Int, Int)]],
    steps : List[(Int, Int)] = List()
  )(implicit MAX_DEPTH : Int) : Option[Table] = {
    PROVE_TABLE_STEP += 1
    if (!steps.isEmpty)
      D.dprintln(steps.reverse.mkString(">"))

    if (System.currentTimeMillis - startTime > Settings.timeout)
      throw new TimeoutException

    if (table.isClosed) {
      D.dprintln("\tClosed!")
      Some(table)
    } else if (table.depth > MAX_DEPTH) {
      D.dprintln("\tmax depth!")
      maxDepthReached = true
      None
    } else {

      val allSteps =
        (for (ic <- inputClauses.indices; idx <- 0 until inputClauses(ic).length) yield {
          (ic, idx)
        }).toList
      val possibleSteps =
        table.nextBranch.head.lit match {
          case PositiveLiteral(a) => literalMap((a.predicate, true))
          case NegativeLiteral(a) => literalMap((a.predicate, false))
          case _ => allSteps
        }

      D.dprintln("[" + PROVE_TABLE_STEP + "] Current table")      

      for ((clause, idx) <- ((-1,-1) :: possibleSteps)) {
        val branch = table.nextBranch
        val remTime = Settings.timeout - (System.currentTimeMillis - startTime)

        val handleResult = 
          if (clause == -1) {
            table.close(remTime)
          } else {
            val copiedClause = inputClauses(clause).copy(PROVE_TABLE_STEP.toString)
            table.extendAndClose(copiedClause, idx, (clause, idx), remTime)
          }

        if (handleResult.isDefined) {
          D.dprintln("\nProveTable...(" + steps.reverse.mkString(",") + "> " +
            (clause, idx) + ") .... (" + PROVE_TABLE_STEP +")")

          if (clause == -1)
            D.dprintln("Closed directly")
          else 
            D.dprintclause(inputClauses(clause), idx)

          proveTable(
            handleResult.get,
            inputClauses,
            literalMap,
            (clause,idx) :: steps) match {
            case Some(nextTable) if nextTable.isClosed => return Some(nextTable)
            case _ => ()
          }
        }
      }
      None
    }
  }

  def getStartingClauses(inputClauses : List[PseudoClause]) = {
    // Some constraints on a good starting clause:
    // (a) We only need to consider positive starting clauses
    // (b) We do not wish to start with a unit clause

    val candidateStartClauses = inputClauses.filterNot(_.isUnit).filter(_.isNegative)

    if (candidateStartClauses.isEmpty)
      throw new Exception("Only unit or negative clauses!")

    if (Settings.start_clause.isDefined)
      List(candidateStartClauses(Settings.start_clause.get))
    else 
      candidateStartClauses
  }


  def prove(inputClauses : List[PseudoClause]) = {
    var result = None : Option[Table]
    var startClause = 0

    startTime = System.currentTimeMillis

    // TODO: Begin by trying to close the unit-clause tableaux (with weak connections I guess)
    val unitClauses = inputClauses.filter(_.length == 1).map(_.toPseudoLiterals().head)

    if (!unitClauses.isEmpty) {
      D.dprintln("UnitClauses:")
      for (uc <- unitClauses)
        D.dprintln("\t" + uc)
    }


    // Lets build up a structure which contains all clauses with a specific literal (and negation)
    // Map[Predicate, Negated] => List((Int, Int))
    val literalMap = MMap() : MMap[(String, Boolean), List[(Int, Int)]]
    for ((ic, i) <- inputClauses.zipWithIndex) {
      for ((pl, j) <- ic.zipWithIndex) {
        pl match {
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
          case PositiveEquation(lhs, rhs) => {}
          case NegativeEquation(lhs, rhs) => {}            
        }
      }
    }

    D.dboxprintln("LiteralMap")
    for ((k, v) <- literalMap)
      D.dprintln(k + " -> " + v.mkString(", "))
    D.dprintln("\n")

    val startClauses = getStartingClauses(inputClauses)

    D.dprintln("Starting Clauses:")
    for (sc <- startClauses)
      D.dprintln("\t" + sc)

    Timer.measure("Prove") {
      // We have to try all input clauses
      var maxDepth = 5
      maxDepthReached = true
      while (!result.isDefined && maxDepthReached) {
        // while (!result.isDefined && startClause < startClauses.length) {
        maxDepthReached = false
        maxDepth += 1
        var startClause = 0
        while (startClause < startClauses.length && !result.isDefined) {
          val iClause= startClauses(startClause)
          val str = "   Start Clause (" + startClause + "): " + iClause + "   "
          D.dboxprintln(str, "YELLOW")
          if (Settings.progress_print)
            println("PROGRESS(" + maxDepth + "." + startClause + ")")          

          // We need to start with all unit clauses          
          val table = Table.create(iClause, unitClauses)
          // D.dprintln(table.toString)

          result = proveTable(table, inputClauses, literalMap.toMap)(maxDepth)
          startClause += 1
        }
      }
    }
    result
  }
}
