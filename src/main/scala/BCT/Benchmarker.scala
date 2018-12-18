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

  def testDir(dir : String, output : String, timeout : Long = 5000) = {
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


  def testFile(problem : String) = {
    D.debug = true
    Parser.tptp2Internal(problem) match {
      case None => ()
      case Some(pseudoClauses) => {
        println("Parsed")
        println("PseudoClauses:")
        for (pc <- pseudoClauses) {
          println(pc)
        }

        val start = System.currentTimeMillis
        try {
          Timer.measure("Prove") {
            Prover.prove(pseudoClauses, 5000) match {
              case None => {
                println("No proof found...")
              }
              case Some(table) => {
                println(table)
              }
            }
          }
        } catch {
          case e : Exception => {
            println("Exception: " + e)
            e.printStackTrace()
          }
        }
        
        val stop = System.currentTimeMillis
        println(Timer)
        println((stop - start) + "ms")
      }
    }
  }

  def run(p : String, timeout : Long) = {
    D.debug = false
    val Some(pseudoClauses) = Parser.tptp2Internal(p)
    Prover.prove(pseudoClauses, timeout)
  }
}
