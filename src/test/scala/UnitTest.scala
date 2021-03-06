package bct

import org.scalatest._

class UnitTest extends FunSuite with DiagrammedAssertions {

  val TIMEOUT = 1000

  // predicates
  val P = "P"
  val R = "R"
  val Pb = "P"

  // functions
  val f = "f"
  val g = "g"

  // terms
  val a = Term("a", 1)
  val b = Term("b", 2)
  val c = Term("c", 3)

  // variables
  val X = Term("X", 4, true)

  // TODO: Should we allow to have two terms which have same name, but different arguments?
  test ("Term") {
    val a2 = Term("a", 5)
    val bU = Term("b", 6, true)
    val bU2 = Term("b", 7, true)
    assert(a == a2)
    assert(a != b)
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
      pl_p_Pab.equation
    }

    val p_aEQc = PositiveEquation(a, c)
    val n_aEQc = NegativeEquation(c, a)
    val pl_p_aEQc = PseudoLiteral(p_aEQc)
    val pl_n_aEQc = PseudoLiteral(n_aEQc)
    assert((pl_p_aEQc.equation.lhs, pl_p_aEQc.equation.rhs) == (a, c))
    assert((pl_n_aEQc.equation.lhs, pl_n_aEQc.equation.rhs) == (c, a))        
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
    val lit_p_Pab = PositiveLiteral(Pab)
    val lit_n_Pab = NegativeLiteral(Pab)    

    val lits = List(lit_p_Pab, lit_n_Pab)
    
    val pc = PseudoClause(List(), lits, Order.Empty)
    assert(pc.length == 2)
    assert(!pc.isEmpty)

    for ((pla, plb) <- lits zip pc)
      assert(pla == plb)

    val pc2 = PseudoClause(List(), lits, Order.Empty)
    assert(pc == pc2)

    val empty_pc = PseudoClause(List(), List(), Order.Empty)
    assert(empty_pc == PseudoClause.Empty)
    assert(empty_pc.isEmpty)
  }

  test ("Domains") {
    val d = Domains(Map())
    assert(d == Domains.Empty)

    val d1 = Domains(Map(X -> Set(a,b,X)))
    val d2 = Domains(Map(b -> Set(b), a -> Set(a)))
    val dd = Domains(Map(X -> Set(a, b, X), a -> Set(a), b -> Set(b)))
    assert(d1.extend(d2) == dd)
  }

  test ("Order") {
    val ord = Order(List(c, X, b, a))
    val dom = ord.toDomains()
    assert(dom(a) contains a)
    assert(!(dom(a) contains b))
    assert(!(dom(a) contains c))

    assert(!(dom(b) contains a))
    assert(dom(b) contains b)
    assert(!(dom(b) contains c))

    assert(dom(X) contains a)
    assert(dom(X) contains b)
    assert(!(dom(X) contains c))
  }

  // test ("Branch") {
  //   // Close simple branch P(a) -> !P(a)
  //   val pl_a = PseudoLiteral(PositiveLiteral(Atom(P, List(a))))
  //   val pl_na = PseudoLiteral(NegativeLiteral(Atom(P, List(a))))
  //   val branch0 = Branch(pl_na, true)
  //   assert(branch0.length == 1)
  //   // assert(!(branch0.tryClose(TIMEOUT)).isDefined)

  //   val branch1 = branch0.extend(pl_a, Order.Empty)
  //   assert(branch1.length == 2)
  //   // assert(branch1.tryClose(TIMEOUT).isDefined)

  //   // Close slightly more complex branch P(X,X) -> (f() = a ^ f()=b)::!P(a,b)
  //   val feq1 = FunEquation(f, List(), a)
  //   val feq2 = FunEquation(f, List(), b)
  //   val order = Order(List(X, a, b))

  //   val br = Branch(List(
  //     PseudoLiteral(PositiveLiteral(Atom(P, List(X, X)))),
  //     PseudoLiteral(List(feq1, feq2), NegativeLiteral(Atom(P, List(a,b))))
  //   ), order, true)
  //   // assert(br.tryClose(TIMEOUT).isDefined)
  // }
}
