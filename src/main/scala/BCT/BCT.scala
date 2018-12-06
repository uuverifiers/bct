package bct

object BCT extends App {
  Timer.measure("Rest") { 
    Prover.prove(Ex1)
  }
}
