package bct

import scala.collection.mutable.{Map => MMap}

class TimeoutException extends Exception

object Prover {
  var startTime : Long = 0
  var maxDepthReached = false
  var maxDepth = 0
  var PROVE_TABLE_STEP = 0
  var startClause = 0

  var breuSolver = new breu.Solver[Term, String](Settings.solver_bits)

  def reset() = {
    startTime = 0
    maxDepthReached = false
    maxDepth = 0
    PROVE_TABLE_STEP = 0
    startClause = 0
  }

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

    if (Settings.timeout.isDefined && System.currentTimeMillis - startTime > Settings.timeout.get) {
      throw new TimeoutException
    }

    if (table.isClosed) {
      D.dlargeboxprintln("Closed!")
      Some(table)
    } else if (table.depth > MAX_DEPTH) {
      // D.dprintln("\tmax depth (" + MAX_DEPTH + ") reached")
      maxDepthReached = true
      None
    } else {
      val allSteps =
        (for (ic <- inputClauses.indices; idx <- 0 until inputClauses(ic).length) yield {
          (ic, idx)
        }).toList


      val possibleSteps =
        if (Settings.hard_coded.isDefined) {
          if (steps.length >= Settings.hard_coded.get.length) {
            D.dlargeboxprintln("Run out of hard_coded steps...")
            List()
          } else {
              List(Settings.hard_coded.get(steps.length))
          }
        } else {
          (-1, -1) :: (
            table.nextBranch.head.lit match {
              case PositiveLiteral(a) => {
                literalMap.getOrElse( (a.predicate, true), List())
              }
              case NegativeLiteral(a) => literalMap.getOrElse( (a.predicate, false), List())

              case PositiveEquation(_, _) | NegativeEquation(_, _) if Settings.extend_equalities => allSteps
              case _ => List()
            }
          )
        }

      if (Settings.debug) {
        if (Settings.hard_coded.isDefined) {
          D.dboxprintln("Hard-coded: " + possibleSteps.head)
        } else {
          D.dboxprintln("Possible steps: (" + MAX_DEPTH + ")")
          for (ps <- possibleSteps)
            println("\t" + ps)
        }
      }

      for ((clause, idx) <- possibleSteps) {
        // println("Trying..." + (clause , idx))
        val branch = table.nextBranch
        val remTime =
          if (Settings.timeout.isDefined)
            Settings.timeout.get - (System.currentTimeMillis - startTime)
          else
            throw new Exception("No timeout defined")

        val handleResult = 
          if (clause == -1) {
            table.close(remTime)
          } else {
            val copiedClause = inputClauses(clause).copy(CUR_PROVE_TABLE_STEP.toString)
            table.extendAndClose(copiedClause, idx, (clause, idx), remTime)
          }

        if (handleResult.isDefined) {
          // If this worked that means we have pushed one subproblem
          // D.dboxprintln("Success!")
          if (clause == -1)
            D.dboxprintln("[" + (">"*steps.length) + CUR_PROVE_TABLE_STEP + "] Closing Directly")
          else
            D.dprintclause(inputClauses(clause), idx, "[" + (">"*steps.length) + CUR_PROVE_TABLE_STEP + "] ")                        

          proveTable(
            handleResult.get,
            inputClauses,
            literalMap,
            (clause,idx) :: steps) match {
            case Some(nextTable) => {
              assert(nextTable.isClosed)
              return Some(nextTable)
            }
            case None => {
              D.cPrintln(D.colorString(" " + (clause, idx) + " fail", "RED"))
              breuSolver.pop()
              if (Settings.essential)
                return None
            }
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

    if (Settings.start_clause.isDefined) {
      List(inputClauses(Settings.start_clause.get))
    } else {
      // val candidateClauses =  inputClauses.filter(_.isNegative)
      // if (candidateClauses.isEmpty)
      //   throw new Exception("Only unit or negative clauses!")
      // candidateClauses
      inputClauses
    }
  }


  def prove(inputClauses : List[PseudoClause], globalConstants : Set[Term]) = Timer.measure("prove") {
    var result = None : Option[Table]

    reset()

    startTime = System.currentTimeMillis
    Timer.measure("breuSolver.restart") {
      breuSolver.restart()
    }

    // TODO: Begin by trying to close the unit-clause tableaux (with weak connections I guess)
    //       Write a test-case for this!
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
          // TODO: When do we expand with positive or negative equations?
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
    maxDepth = Settings.start_max_depth + unitClauses.length
    maxDepthReached = true

    while (!result.isDefined && maxDepthReached) {
      // TODO: Lets try and print breuSolver here to make sure its empty?
      // TOOD: This would require us to add constants every loop...
      maxDepthReached = false
      maxDepth += 1
      D.dlargeboxprintln("INCREASING MAX DEPTH: " + maxDepth)
      var startClause = 0
      while (startClause < startClauses.length && !result.isDefined) {
        val iClause= startClauses(startClause)
        val str = "   Start Clause (" + startClause + "): " + iClause + "   "
        D.dboxprintln(str, "YELLOW")

        // We need to start with all unit clauses
        val table = Timer.measure("Creating Table") {
          if (Settings.add_unit) 
            Table.create(iClause, unitClauses)
          else
            Table.create(iClause)
        }

        result = proveTable(table, inputClauses, literalMap.toMap)(maxDepth)
        startClause += 1
      }
    }

    result
  }
}
