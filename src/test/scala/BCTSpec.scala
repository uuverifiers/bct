package bct

import org.scalatest._

class BCTSpec extends FunSuite with DiagrammedAssertions {
  test("BCT should start with B") {
    assert("bct".startsWith("b"))
  }

  test("Atoms") {
    val a = Atom("R", List("a"))
    val b = Atom("R", List("b"))

    val feq = new FunEquation("f", List("a1", "a2"), "b")
  }

  test ("Prover can run example") {

    Prover.prove(2)
    println("\n\nEND OF TEST\n\n")
  }

}
