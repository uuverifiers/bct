package bct

import org.scalatest._
import java.io.File

class BCTParser extends FunSuite with DiagrammedAssertions {
  val dir = new File(getClass.getResource("/parser/").toURI())

  def parseFile(problem : String) = {
    Parser.tptp2Internal(dir.toString + "/" + problem).get
  }

  val Ax0 = Term("x_0", true)
  val Ax1 = Term("x_1", true)
  val Ax2 = Term("x_2", true)    

  val Ex0 = Term("c_0")
  val Ex1 = Term("c_1")
  val Ex2 = Term("c_2")  


  test ("equiv+1") {
    val pc1 = {
      val lit1 = PseudoLiteral(NegativeLiteral(Atom("subset/2", List(Ax0, Ax1))))

      val feq2 = List(FunEquation("union/2", List(Ax0, Ax1), Ex0))
      val lit2 = PseudoLiteral(feq2, PositiveEquation(Ex0, Ax1))
      PseudoClause(List(lit1, lit2))
    }
    println("EQUIV1! -- >" + List(pc1))
    assert(List(pc1) == parseFile("equiv+1.p"))
  }

// % {union/2(∀x_0,∀x_1) = ∃c_0::(~member/2(∀x_2,∃c_0))}, {member/2(∀x_2,∀x_0)}, {member/2(∀x_2,∀x_1)}
// % {(~member/2(∀x_2,∀x_0))}, {union/2(∀x_0,∀x_1) = ∃c_1::member/2(∀x_2,∃c_1)}
// % {(~member/2(∀x_2,∀x_1))}, {union/2(∀x_0,∀x_1) = ∃c_2::member/2(∀x_2,∃c_2)}

  test ("equiv+2") {
    val pc1 = {
      val feq1 = List(FunEquation("union/2", List(Ax0, Ax1), Ex0))
      val lit1 = PseudoLiteral(feq1, NegativeLiteral(Atom("member/2", List(Ax2, Ex0))))

      val lit2 = PseudoLiteral(PositiveLiteral(Atom("member/2", List(Ax2, Ax0))))

      val lit3 = PseudoLiteral(PositiveLiteral(Atom("member/2", List(Ax2, Ax1))))

      PseudoClause(List(lit1, lit2, lit3))
    }

    val pc2 = {
      val lit1 = PseudoLiteral(NegativeLiteral(Atom("member/2", List(Ax2, Ax0))))

      val feq2 = List(FunEquation("union/2", List(Ax0, Ax1), Ex1))
      val lit2 = PseudoLiteral(feq2, PositiveLiteral(Atom("member/2", List(Ax2, Ex1))))      

      PseudoClause(List(lit1, lit2))
    }

    val pc3 = {
      val lit1 = PseudoLiteral(NegativeLiteral(Atom("member/2", List(Ax2, Ax1))))

      val feq2 = List(FunEquation("union/2", List(Ax0, Ax1), Ex2))
      val lit2 = PseudoLiteral(feq2, PositiveLiteral(Atom("member/2", List(Ax2, Ex2))))      

      PseudoClause(List(lit1, lit2))
    }        

    assert(List(pc1, pc2, pc3) == parseFile("equiv+2.p"))
  }
  

  // fof(union_defn,axiom,
  //   ( ! [B,C,D] :
  //       ( member(D,union(B,C))
  //     <=> ( member(D,B)
  //         | member(D,C) ) ) )).

  // [{(~member/2(∀x_2,∀x_0))} v {(~member/2(∀x_2,∀x_1))} v {union/2(∀x_0,∀x_1) = ∃c_3::member/2(∀x_2,∃c_3)}],
  // [{union/2(∀x_0,∀x_1) = ∃c_2::(~member/2(∀x_2,∃c_2))} v {(~member/2(∀x_2,∀x_0))}]
  // [{union/2(∀x_0,∀x_1) = ∃c_2::(~member/2(∀x_2,∃c_2))} v {(~member/2(∀x_2,∀x_1))}])



  test ("equiv+3") {
    val pc1 = {
      val lit1 = PseudoLiteral(NegativeEquation(Ax0, Ax1))
      val lit2 = PseudoLiteral(PositiveLiteral(Atom("subset/2", List(Ax0, Ax1))))
      PseudoClause(List(lit1, lit2))
    }
    val pc2 = {
      val lit1 = PseudoLiteral(NegativeEquation(Ax0, Ax1))
      val lit2 = PseudoLiteral(PositiveLiteral(Atom("subset/2", List(Ax1, Ax0))))
      PseudoClause(List(lit1, lit2))      
    }

    val pc3 = {
      val lit1 = PseudoLiteral(NegativeLiteral(Atom("subset/2", List(Ax0, Ax1))))
      val lit2 = PseudoLiteral(NegativeLiteral(Atom("subset/2", List(Ax1, Ax0))))
      val lit3 = PseudoLiteral(PositiveEquation(Ax0, Ax1))
      PseudoClause(List(lit1, lit2, lit3))
    }    

    assert(List(pc1, pc2, pc3) == parseFile("equiv+3.p"))
  }
}
