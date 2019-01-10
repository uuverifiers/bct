package bct

import scala.collection.mutable.{Map => MMap}

class TimeoutException extends Exception

object Prover {
  var startTime : Long = 0
  var maxDepthReached = false
  var maxDepth = 0
  var PROVE_TABLE_STEP = 0
  var startClause = 0

  def proveTable(
    table : Table,
    inputClauses : List[PseudoClause],
    literalMap : Map[(String, Boolean), List[(Int, Int)]],
    steps : List[(Int, Int)] = List()
  )(implicit MAX_DEPTH : Int) : Option[Table] = {
    PROVE_TABLE_STEP += 1    
    val CUR_PROVE_TABLE_STEP = PROVE_TABLE_STEP
    if (!steps.isEmpty)
      D.dprintln(steps.reverse.mkString(">"))
    if (Settings.debug) {
      D.dprintln("[" + CUR_PROVE_TABLE_STEP + "] Current table")
      if (Settings.full_table)
        println(table)
      else
        println(table.simple)
    }

    if (System.currentTimeMillis - startTime > Settings.timeout)
      throw new TimeoutException

    if (table.isClosed) {
      D.dboxprintln("Closed!")
      Some(table)
    } else if (table.depth > MAX_DEPTH) {
      D.dprintln("\tmax depth (" + MAX_DEPTH + ") reached")
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

      if (Settings.debug) {
        D.dboxprintln("Possible steps: (" + MAX_DEPTH + ")")
        for (ps <- possibleSteps)
          println("\t" + ps)
      }

      for ((clause, idx) <- ((-1,-1) :: possibleSteps)) {
        val branch = table.nextBranch
        val remTime = Settings.timeout - (System.currentTimeMillis - startTime)

        val handleResult = 
          if (clause == -1) {
            D.dboxprintln("[" + CUR_PROVE_TABLE_STEP + "] Closing Directly")
            table.close(remTime)
          } else {
            D.dprintclause(inputClauses(clause), idx, "[" + CUR_PROVE_TABLE_STEP + "] ")            
            val copiedClause = inputClauses(clause).copy(CUR_PROVE_TABLE_STEP.toString)
            table.extendAndClose(copiedClause, idx, (clause, idx), remTime)
          }

        if (handleResult.isDefined) {
          D.dboxprintln("Success!")
          // D.dprintln("\nProveTable...(" + steps.reverse.mkString(",") + "> " +
          //   (clause, idx) + ") .... (" + CUR_PROVE_TABLE_STEP +")")

          proveTable(
            handleResult.get,
            inputClauses,
            literalMap,
            (clause,idx) :: steps) match {
            case Some(nextTable) if nextTable.isClosed => return Some(nextTable)
            case None if Settings.essential => return None
            case _ => ()
          }
        } else {
          D.dprintln("Fail...")
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

    // We have to try all input clauses
    maxDepth = Settings.start_max_depth
    maxDepthReached = true

    Timer.measure("Prove") {
      while (!result.isDefined && maxDepthReached) {
        maxDepthReached = false
        maxDepth += 1
        D.dlargeboxprintln("INCREASING MAX DEPTH: " + maxDepth)
        var startClause = 0
        while (startClause < startClauses.length && !result.isDefined) {
          val iClause= startClauses(startClause)
          val str = "   Start Clause (" + startClause + "): " + iClause + "   "
          D.dboxprintln(str, "YELLOW")

          // We need to start with all unit clauses
          val table = Table.create(iClause, unitClauses)

          result = proveTable(table, inputClauses, literalMap.toMap)(maxDepth)
          startClause += 1          
        }
      }
    }

    result
  }
}
