package bct

abstract class Example {
  val clauses : Int
  def clause(i : Int) : PseudoClause

  def getInputClause(i : Int) = {
    if (i >= 0 || i< clauses)
      clause(i)
    else
      throw new Exception("No such input clause")
  }

  var allCount = -1
  var exCount = -1

  def newEx() = {
    exCount += 1
    Term("c_" + exCount, false)
  }

  def newAll() = {
    allCount += 1

    Term("x_" + allCount, true)
  }  

}

// {R(a), -R(a)
object Ex0 extends Example {
  val clauses = 2
  def clause(i : Int) = {
    i match {
      case 0 => PseudoClause(List(PseudoLiteral(PositiveLiteral(Atom("R", List(Term("a")))))))
      case 1 => PseudoClause(List(PseudoLiteral(NegativeLiteral(Atom("R", List(Term("a")))))))
    }
  }
}

// {R(X) v R(f(X)), -R(X) v -R(f(f(X)))}
object Ex1 extends Example {
  val pred = "R"
  val fun = "f"

  val clauses = 2
  def clause(i : Int) =
    i match {
      case 0 => {
        val pred = "R"
        val fun = "f"
        val x = newAll()
        val a1 = Atom(pred, List(x)) // R(x)

        val c1 = newEx()
        val feq1 = FunEquation(fun, List(x), c1)
        val a2 = Atom(pred, List(c1))

        val l1 = PseudoLiteral(PositiveLiteral(a1))
        val l2 = PseudoLiteral(List(feq1), PositiveLiteral(a2))
        PseudoClause(List(l1, l2))
      }
      case 1 => {

        val x = newAll()
        val a1 = Atom(pred, List(x)) // R(x)

        val c1 = newEx()
        val feq1 = FunEquation(fun, List(x), c1)
        val a2 = Atom(pred, List(c1))

        val c2 = newEx()
        val feq2 = FunEquation(fun, List(c1), c2)
        val a3 = Atom(pred, List(c2))

        val l1 = PseudoLiteral(NegativeLiteral(a1))
        val l2 = PseudoLiteral(List(feq1, feq2), NegativeLiteral(a3))
        PseudoClause(List(l1, l2))
      }
    }
}


// {a = b, a != b}
object Ex2 extends Example {
  val r = "R"
  val a = Term("a")
  val b = Term("b")  
  val ra = PositiveLiteral(Atom(r, List(a)))
  val nrb = NegativeLiteral(Atom(r, List(b)))
  val aEQb = PositiveEquation(a, b)
  val aNEQb = NegativeEquation(a, b)  

  val clauses = 2
  def clause(i : Int) =
    i match {
      case 0 => PseudoClause(List(PseudoLiteral(aEQb)))
      case 1 => PseudoClause(List(PseudoLiteral(aNEQb)))
    }
}


// featers
object Ex3 extends Example {
  val feathers = "feathers"
  val bird = "bird"
  val t = Term("t")

  val clauses = 3
  def clause(i : Int) =
    i match {
      case 0 => {
        val x = newAll()
        PseudoClause(List(PseudoLiteral(NegativeLiteral(Atom(bird, List(x)))), PseudoLiteral(PositiveLiteral(Atom(feathers, List(x))))))
      }
      case 1 => PseudoClause(List(PseudoLiteral(PositiveLiteral(Atom(bird, List(t))))))
      case 2 => PseudoClause(List(PseudoLiteral(NegativeLiteral(Atom(feathers, List(t))))))
    }
}
