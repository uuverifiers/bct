package bct

import org.scalatest._

class BCTUnit extends FunSuite with DiagrammedAssertions {
  // predicates
  val P = "P"
  val R = "R"
  val Pb = "P"

  // functions
  val f = "f"
  val g = "g"

  // terms
  val a = Term("a")
  val b = Term("b")
  val c = Term("c")

  test ("Term") {
    val a2 = Term("a")
    val bU = Term("b", true)
    val bU2 = Term("b", true)
    assert(a == a2)
    assert(a != b)
    assert(b != bU)
    assert(bU == bU2)
  }

  test ("Atom") {
    val a1 = Atom(P, List(a, b))
    assert(a1.terms == Set(a, b))
    val a2 = Atom(R, List())
    assert(a2.terms == Set())
    val a3 = Atom(P, List(a,b))
    assert(a1 == a3)
    val a4 = Atom(R, List(a,b))
    assert(a1 != a4)
    val a5 = Atom(P, List(b,a))
    assert(a1.terms == a5.terms)
    assert(a1 != a5)
    val a6 = Atom(Pb, List(b,a))
    assert(a5 == a6)
  }

  test ("Literal") {
    val Pab = Atom(P, List(a, b))
    val p_Pab = PositiveLiteral(Pab)
    val b_Pab = NegativeLiteral(Pab)
    assert(p_Pab.isComplementary(b_Pab))
    assert(b_Pab.isComplementary(p_Pab))
    assert(p_Pab.terms == Set(a,b))
    assert(p_Pab.terms == b_Pab.terms)

    val p_R = PositiveLiteral(Atom(R, List()))
    val n_R = NegativeLiteral(Atom(R, List()))
    assert(p_R.isComplementary(n_R))
    assert(n_R.isComplementary(p_R))
    assert(!p_Pab.isComplementary(n_R))

    val p_aEQc = PositiveEquation(a, c)
    val p_cEQa = PositiveEquation(c, a)
    assert(p_aEQc.terms == Set(a,c))
    assert(p_aEQc.terms == p_cEQa.terms)
    assert(p_aEQc.isPositiveEquation)
    assert(!p_aEQc.isNegativeEquation)

    val n_aEQc = NegativeEquation(a, c)
    val n_cEQa = NegativeEquation(c, a)
    assert(n_aEQc.terms == Set(a,c))
    assert(n_aEQc.terms == n_cEQa.terms)
    assert(!n_aEQc.isPositiveEquation)
    assert(n_aEQc.isNegativeEquation)
    assert(!p_aEQc.isComplementary(n_aEQc))
  }

  test ("FunEquation") {
    // Nothing to test
  }

  test ("PseudoLiteral") {

    val Pab = Atom(P, List(a, b))
    val p_Pab = PositiveLiteral(Pab)
    val n_Pab = NegativeLiteral(Pab)    
    val pl_p_Pab = PseudoLiteral(p_Pab)
    val pl_n_Pab = PseudoLiteral(n_Pab)
    assert(pl_p_Pab.lit == p_Pab)
    assert(pl_p_Pab.atom == Pab)
    assert(pl_n_Pab.atom == Pab)
    assert(pl_n_Pab.isComplementary(pl_p_Pab))
    assert(pl_n_Pab.funEquations.isEmpty)
    assertThrows[Exception]{
      pl_p_Pab.terms
    }

    val p_aEQc = PositiveEquation(a, c)
    val n_aEQc = NegativeEquation(c, a)
    val pl_p_aEQc = PseudoLiteral(p_aEQc)
    val pl_n_aEQc = PseudoLiteral(n_aEQc)
    assert(pl_p_aEQc.terms == (a, c))
    assert(pl_n_aEQc.terms == (c, a))
    assert(!pl_n_aEQc.isComplementary(pl_p_aEQc))

    assertThrows[Exception]{
      pl_p_aEQc.atom
    }

    val fa_c = FunEquation(f, List(a), c)
    val pl_fa_c_p_Pab = PseudoLiteral(List(fa_c), p_Pab)
    assert(pl_fa_c_p_Pab.lit == p_Pab)
    assert(pl_fa_c_p_Pab.funEquations == List(fa_c))
    val pl_fa_c_p_Pab2 = PseudoLiteral(List(fa_c), p_Pab)
    assert(pl_fa_c_p_Pab == pl_fa_c_p_Pab2)
    assert(pl_fa_c_p_Pab != pl_p_Pab)
  }

  test ("PseudoClause") {
    val Pab = Atom(P, List(a, b))
    val p_Pab = PositiveLiteral(Pab)
    val n_Pab = NegativeLiteral(Pab)    
    val pl_p_Pab = PseudoLiteral(p_Pab)
    val pl_n_Pab = PseudoLiteral(n_Pab)

    val plList = List(pl_p_Pab, pl_n_Pab)
    val pc = PseudoClause(plList)
    assert(pc.length == 2)
    assert(!pc.isEmpty)

    for ((pla, plb) <- plList zip pc)
      assert(pla == plb)

    val pc2 = PseudoClause(List(pl_p_Pab, pl_n_Pab))
    assert(pc == pc2)

    val pc3 = PseudoClause(List(pl_p_Pab))
    val pc4 = PseudoClause(pl_p_Pab)
    assert(pc3 == pc4)

    val empty_pc = PseudoClause(List())
    assert(empty_pc == PseudoClause.EmptyPseudoClause)
    assert(empty_pc.isEmpty)
  }
}
