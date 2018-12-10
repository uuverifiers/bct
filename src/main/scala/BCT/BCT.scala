package bct

import scala.collection.mutable.Map

object BCT extends App {
  // Timer.measure("Rest") {
  //   Prover.prove(Ex1)
  // }

  val problems = List("SYN003-1.006.p", "SYN008-1.p", "SYN009-1.p", "SYN009-2.p")
  val times = Map() : Map[String, Long]
  val exceptions = Map() : Map[String, String]
  val results = Map () : Map[String, String]
  for (problem <- problems) {
    val fileName = "Problems/SYN/" + problem
    Parser.tptp2Internal(fileName) match {
      case None => println("File \"" + fileName + "\" not found.")
      case Some(pseudoClauses) => {
        println("PseudoClauses:")
        for (pc <- pseudoClauses) {
          println(pc)
        }

        val start = System.currentTimeMillis
        try {
          Timer.measure("Prove") {
            Prover.prove(pseudoClauses) match {
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
