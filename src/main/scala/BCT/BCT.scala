package bct

import scala.collection.mutable.Map
import java.io.File 
object BCT extends App {
  // Timer.measure("Rest") {
  //   Prover.prove(Ex1)
  // }


  def testDir() = {

    val directory = new File("Problems/subSYN")

    val problems = directory.listFiles.map(_.toString).filter(_.contains("-")).filter(_.endsWith(".p"))
    val problemCount = problems.length

    val times = Map() : Map[String, Long]
    val exceptions = Map() : Map[String, String]
    val results = Map () : Map[String, String]
    var no = 0
    for (problem <- problems) {
      no += 1
      println(problem + "(" + no + "/" + problemCount + ")")
      Parser.tptp2Internal(problem) match {
        case None => println("File \"" + problem + "\" not found.")
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
                  results += problem -> "none"
                  println("No proof found...")
                }
                case Some(table) => {
                  results += problem -> "some"
                  println(table)
                }
              }
            }
          } catch {
            case e : Exception => exceptions += problem -> e.toString
          }
          
          val stop = System.currentTimeMillis
          println(Timer)
          times += problem -> (stop - start)
        }
      }
    }

    for (p <- problems) {
      println("<<<" + p + ">>>")
      println(exceptions.getOrElse(p, "No exceptions"))
      println(results.getOrElse(p, "No result"))
      println(times(p))
    }
  }

  def testFile() = {
    val problem = "Problems/subSYN/SYN001-1.005.p"
    Parser.tptp2Internal(problem) match {
      case None => println("File \"" + problem + "\" not found.")
      case Some(pseudoClauses) => {
        println("Parsed")
        println("PseudoClauses:")
        for (pc <- pseudoClauses) {
          println(pc)
        }

        val start = System.currentTimeMillis
        try {
          Timer.measure("Prove") {
            Prover.prove(pseudoClauses, 60000) match {
              case None => {
                println("No proof found...")
              }
              case Some(table) => {
                println(table)
              }
            }
          }
        } catch {
          case e : Exception => println("Exception: " + e)
        }
        
        val stop = System.currentTimeMillis
        println(Timer)
        println((stop - start) + "ms")
      }
    }
  }

  testFile()
}
