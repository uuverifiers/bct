package bct
import scala.collection.mutable.{Map => MMap}

object Benchmarker {
  def run(problem : String) : String = {
    val Some(pseudoClauses) = Parser.tptp2Internal(problem)
    val strs = List(
      "+-------------------+",
      "|  PseudoClauses:   |",
      "+-------------------+") ++
    (for ((pc, i) <- pseudoClauses.zipWithIndex) yield {
      "(%3d)\t%s".format(i, pc)
    })

    D.dprintln(strs.mkString("\n"))
    D.dprintln("\n\n")

    val start = System.currentTimeMillis
    try {
      Timer.measure("Prove") {
        val ret = Prover.prove(pseudoClauses)
        val stop = System.currentTimeMillis
        if (Settings.debug) {
          println((stop - start) + "ms")
        }
        ret match {
          case None => {
            D.dprintln("Incomplete search")
            "UNKNOWN"
          }
          case Some(table) => {
            D.dprintln(table.toString)
            "SAT"
          }
        }
      }

    } catch {
      case to : breu.TimeoutException => {
        D.dprintln("Timeout")
        "TIMEOUT"
      }

      case e : Exception => {
        println("Exception: " + e)
        println("\t" + e.getClass)
        e.printStackTrace()
        "ERROR"
      }
    } finally {
      if (Settings.progress_print)
        println("PROGRESS(" + Prover.maxDepth + "." + (Prover.startClause) + ")")
      if (Settings.debug || Settings.time) {
        println(Timer)
      }
    }
  }
}
