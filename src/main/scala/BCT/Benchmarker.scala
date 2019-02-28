package bct
import scala.collection.mutable.{Map => MMap}


object Benchmarker {

  type BenchmarkData = Map[String, (Int, Long)]

  def benchmark(problem : String, count : Int = 1) = {

    var result = ""
    var functions = Set() : Set[String]

    val times = MMap() : MMap[String, List[Long]]
    val calls = MMap() : MMap[String, List[Int]]

    for (i <- 0 until count) yield {
      Timer.reset
      val (res, data) = runOnce(problem)
      if (result == "") {
        result = res
        functions = data.keys.toSet
      } else {
        assert(result == res)
        assert(functions == data.keys.toSet)
      }

      for (f <- functions) {
        val (c, t) : (Int, Long) = data(f) 
        calls += f -> (c :: calls.getOrElse(f, List()))
        times += f -> (t :: (times.getOrElse(f, List())))
      }
    }

    var totalTime = 0.0

    val padding = functions.map(_.length).max
    for (f <- functions) {
      // println(f + "\n\t" + calls(f).mkString(",") + "\n\t" + times(f).map(_.toDouble / 1000000.0).mkString(","))
      println(f + (" "*(padding-f.length)) + "\t" + (calls(f).sum/count) + "\t" + (times(f).map(_.toDouble / 1000000.0).sum/count))
      totalTime += times(f).map(_.toDouble / 1000000.0).sum/count
    }

    println("Total: " + totalTime)
    result
  }

  def runOnce(problem : String) : (String, BenchmarkData)  = {
    val Some((pseudoClauses, globalConstants)) = Parser.tptp2Internal(problem)
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
    val ret = 
      try {
        val ret = Prover.prove(pseudoClauses, globalConstants)
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

      } catch {
        case to : bct.TimeoutException => {
          D.dprintln("Timeout")
          "TIMEOUT"
        }
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
    (ret, Timer.data())
  }

  def run(problem : String)  = {
    runOnce(problem)._1
  }  
}
