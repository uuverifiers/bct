package bct

object BCT extends App {
  // Timer.measure("Rest") { 
  //   Prover.prove(Ex1)
  // }

  val problem = "SYN003-1.006.p"
  val fileName = "Problems/SYN/" + problem

  val pseudoClauses = Parser.tptp2Internal(fileName)

  println("PseudoClauses:")
  for (pc <- pseudoClauses)
    println(pc)
}
