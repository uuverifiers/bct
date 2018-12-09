package bct

import org.scalatest._

class BCTSpec extends FunSuite with DiagrammedAssertions {
  test ("Ex1 should find proof") {
    assert(Prover.prove(Ex1).isDefined)
  }

  // test ("Ex2 should find proof") {
  //   assert(Prover.prove(Ex2).isDefined)    
  // }

  // test ("Ex3 should find proof") {
  //   assert(Prover.prove(Ex3).isDefined)    
  // }  
}
