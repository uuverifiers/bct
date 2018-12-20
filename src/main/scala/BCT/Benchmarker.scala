package bct
import java.io.{File, FileWriter, BufferedWriter}
import scala.collection.mutable.{Map => MMap}

//
// CSV FORMAT
//
// benchmark, result, time
//
//

object Benchmarker {

  def testDir(dir : String, output : String, timeout : Long) = {
    val directory = new File(dir)
    val problems = directory.listFiles.map(_.toString)
    val problemCount = problems.length

    // FileWriter
    val file = new File(output)
    val bw = new BufferedWriter(new FileWriter(file))

    var no = 0
    for (problem <- problems) {
      no += 1
      println(problem + "(" + no + "/" + problemCount + ")")
      val pseudoClauses = Parser.tptp2Internal(problem)
      if (!pseudoClauses.isDefined) {
        println("File \"" + problem + "\" not found.")
      } else {
        val start = System.currentTimeMillis
        val result = 
          try {
            Prover.prove(pseudoClauses.get, timeout) match {
              case None => "unknown"
              case Some(table) => "sat"
            }
          } catch {
            case to : TimeoutException => "timeout"
            case e : Exception => "error"
          }

        println("RESULT: " + result)
        val stop = System.currentTimeMillis
        bw.write(List(problem, result, (stop - start).toString).mkString(",") + "\n")
        bw.flush()
      }
    }

    bw.close()    
  }  


  def testFile(problem : String, timeout : Long, startClause : Option[Int]) = {
    println("Solving with timeout: " + timeout)
    D.debug = true
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
        Prover.prove(pseudoClauses, timeout, startClause) match {
          case None => {
            println("Incomplete search")
          }
          case Some(table) => {
            println(table)
          }
        }
      }
    } catch {
      case to : TimeoutException => {
        println("Timeout")
      }

      case e : Exception => {
        println("Exception: " + e)
        e.printStackTrace()
      }
    }
    
    val stop = System.currentTimeMillis
    println(Timer)
    println((stop - start) + "ms")
  }

  def run(p : String, timeout : Long) : String = {
    D.debug = false
    val Some(pseudoClauses) = Parser.tptp2Internal(p)

    try {
      Prover.prove(pseudoClauses, timeout) match {
        case Some(_) => "SAT"
        case None => "UNKNOWN"
      }
    } catch {
      case to : TimeoutException => "TIMEOUT"
    }
  }
}
