package bct

import org.scalatest._
import java.io.File

class ParserTest extends FunSuite with DiagrammedAssertions {
  val dir = new File(getClass.getResource("/parser/").toURI())

  def parseFile(problem : String) = {
    Parser.tptp2Internal(dir.toString + "/" + problem).get
  }

  val Ax0 = Term("x_0", 1, true)
  val Ax1 = Term("x_1", 2, true)
  val Ax2 = Term("x_2", 3, true)    

  val Ex0 = Term("c_0", 4)
  val Ex1 = Term("c_1", 5)
  val Ex2 = Term("c_2", 6)  


  // test ("equiv+1") {
  //   val pc1 = {
  //     val lit1 = NegativeLiteral(Atom("subset/2", List(Ax0, Ax1)))

  //     val feq2 = FunEquation("union/2", List(Ax0, Ax1), Ex0)
  //     val lit2 = PositiveEquation(Ex0, Ax1)

  //     val order = Order(List(Ax1, Ax0, Ex0))
  //     PseudoClause(List(feq2), List(lit1, lit2), order)
  //   }

  //   assert(List(pc1) == parseFile("equiv+1.p"))
  // }

  // test ("equiv+2") {
  //   val pc1 = {
  //     val feq1 = FunEquation("union/2", List(Ax0, Ax1), Ex0)
  //     val lit1 = NegativeLiteral(Atom("member/2", List(Ax2, Ex0)))

  //     val lit2 = PositiveLiteral(Atom("member/2", List(Ax2, Ax0)))

  //     val lit3 = PositiveLiteral(Atom("member/2", List(Ax2, Ax1)))

  //     val order = Order(List(Ax2, Ax1, Ax0, Ex0, Ex1, Ex2))
  //     PseudoClause(List(feq1), List(lit1, lit2, lit3), order)
  //   }

  //   val pc2 = {
  //     val lit1 = NegativeLiteral(Atom("member/2", List(Ax2, Ax0)))

  //     val feq2 = FunEquation("union/2", List(Ax0, Ax1), Ex1)

  //     val lit2 = PositiveLiteral(Atom("member/2", List(Ax2, Ex1)))

  //     val order = Order(List(Ax2, Ax1, Ax0, Ex0, Ex1, Ex2))

  //     PseudoClause(List(feq2), List(lit1, lit2), order)
  //   }

  //   val pc3 = {
  //     val lit1 = NegativeLiteral(Atom("member/2", List(Ax2, Ax1)))

  //     val feq2 = FunEquation("union/2", List(Ax0, Ax1), Ex2)

  //     val lit2 = PositiveLiteral(Atom("member/2", List(Ax2, Ex2)))

  //     val order = Order(List(Ax2, Ax1, Ax0, Ex0, Ex1, Ex2))      

  //     PseudoClause(List(feq2), List(lit1, lit2), order)
  //   }        

  //   assert(List(pc1, pc2, pc3) == parseFile("equiv+2.p"))
  // }


  // test ("equiv+3") {
  //   val pc1 = {
  //     val lit1 = NegativeEquation(Ax0, Ax1)
  //     val lit2 = PositiveLiteral(Atom("subset/2", List(Ax0, Ax1)))
  //     val order = Order(List(Ax1, Ax0))
  //     PseudoClause(List(), List(lit1, lit2), order)
  //   }
  //   val pc2 = {
  //     val lit1 = NegativeEquation(Ax0, Ax1)
  //     val lit2 = PositiveLiteral(Atom("subset/2", List(Ax1, Ax0)))
  //     val order = Order(List(Ax1, Ax0))      
  //     PseudoClause(List(), List(lit1, lit2), order)
  //   }

  //   val pc3 = {
  //     val lit1 = PositiveLiteral(Atom("subset/2", List(Ax0, Ax1)))
  //     val lit2 = PositiveLiteral(Atom("subset/2", List(Ax1, Ax0)))
  //     val lit3 = PositiveEquation(Ax0, Ax1)      
  //     val order = Order(List(Ax1, Ax0))      
  //     PseudoClause(List(), List(lit1, lit2, lit3), order)
  //   }

  //   assert(List(pc1, pc2, pc3) == parseFile("equiv+3.p"))
  // }
}
