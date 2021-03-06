package bct

/*
 A branch consists of PseudoLiterals
 */

object Branch {
  def apply(pseudoLiteral : PseudoLiteral, strong : Boolean, unitClauses : Int) : Branch = {
    Branch(List(pseudoLiteral), pseudoLiteral.order, strong, unitClauses)
  }

  def apply(pseudoLiterals : List[PseudoLiteral], order : Order, strong : Boolean, unitClauses : Int) : Branch = {
    Branch(pseudoLiterals, order, false, strong, unitClauses)
  }

  class Conflict
  case class ComplementaryPair(a1 : Atom , a2 : Atom) extends Conflict {
    override def toString() = {
      "C[" + (for ((t1, t2) <- a1.args zip a2.args) yield (t1 + " != " + t2)).mkString(", ") + "]"
    }
  }
  case class InvalidEquation(t1 : Term, t2 : Term) extends Conflict {
    override def toString() = {
      "C[" + t1 + " != " + t2 + "]"
    }
  }

  type Equation = (String, List[Term], Term)
  type Goal = List[List[(Term, Term)]]  
}


case class Branch(
  pseudoLiterals : List[PseudoLiteral],
  order : Order,
  val isClosed : Boolean,
  val strong : Boolean,
  val unitClauses : Int) extends Iterable[PseudoLiteral] {
  assert(pseudoLiterals.length > 0)
  def length = pseudoLiterals.length
  def depth = pseudoLiterals.length
  def iterator = pseudoLiterals.iterator
  override def toString() =
    pseudoLiterals.mkString("<-") + " || " + order + " || " + conflicts.mkString(" v ")

  def simpleString() =
    if (pseudoLiterals.length > 3) 
      pseudoLiterals.take(3).mkString("<-") + "..."
    else
      pseudoLiterals.mkString("<-")

  lazy val closed = Branch(pseudoLiterals, order, true, strong, unitClauses)
  lazy val weak = Branch(pseudoLiterals, order, isClosed, false, unitClauses)

  lazy val conflicts : List[Branch.Conflict] = {
    if (strong) {
      // Only allow contradiction involving last one or two nodes
      val n1 = pseudoLiterals(0)

      // Only use this if branch length > 0
      lazy val n2 = pseudoLiterals(1)

      // Three cases:
      // (1) First node is an inequality, then this inequality must be part of the conflict
      // (2) First node is Positive/NegativeLiteral,
      //     second node is an equality, then first node must be in conflict (using equality)
      // (3) First and second node is a Positive/NegativeLiteral, then they must be in conflict
      // (4) First node is an equality, then this equality must be part of the conflict

      if (n1.lit.isNegativeEquation) {
        // Case 1
        val (lhs, rhs) = (n1.equation.lhs, n1.equation.rhs)
        List(Branch.InvalidEquation(lhs, rhs))
      } else if (pseudoLiterals.length >= 2 && n1.lit.isAtom && n2.lit.isPositiveEquation) {
        // Case 2
        // TODO: Add check that n1 and n3 actually uses equation in n2.
        for (n3 <- pseudoLiterals.drop(2); if n1.isComplementary(n3))
        yield Branch.ComplementaryPair(n1.atom, n3.atom)
      } else if (pseudoLiterals.length >= 2 && n1.isComplementary(n2)) {
        // Case 3
        List(Branch.ComplementaryPair(n1.atom, n2.atom))
      } else if (n1.lit.isPositiveEquation) {
        // Case 4
        this.weak.conflicts
      } else {
        List()
      }
    } else {
      val pairConflicts : List[Branch.Conflict]  =
        (for (
          i1 <- 0 until pseudoLiterals.length;
          i2 <- (i1+1) until pseudoLiterals.length;
          if pseudoLiterals(i1).isComplementary(pseudoLiterals(i2)))
        yield Branch.ComplementaryPair(pseudoLiterals(i1).atom, pseudoLiterals(i2).atom)).toList

      val singleConflict : List[Branch.Conflict] =
        for (pl <- pseudoLiterals; if pl.lit.isNegativeEquation) yield {
          val (lhs, rhs) = (pl.equation.lhs, pl.equation.rhs)
          Branch.InvalidEquation(lhs, rhs)
        }

      pairConflicts ++ singleConflict
    }
  }


  lazy val funEquations = {
    pseudoLiterals.map(_.funEquations).flatten
  }

  lazy val equations = pseudoLiterals.map(_.lit).filter(_.isPositiveEquation)

  lazy val toUnification : List[List[(Term, Term)]] = {
    (for (c <- conflicts) yield {
      c match {
        case Branch.ComplementaryPair(a1, a2) => {
          (for ((arg1, arg2) <- a1.args zip a2.args) yield {
            (arg1, arg2)
          }).toList
        }

        case Branch.InvalidEquation(t1, t2) => {
          List((t1, t2))
        }
      }
    }).toList
  }

  lazy val toBreu : Option[(Domains, List[Branch.Equation], Branch.Goal)] = {
    if (conflicts.length == 0) {
      None
    } else {
      val funEqs = this.funEquations
      val eqs = this.equations
      val goals =
        for (c <- conflicts) yield {
          c match {
            case Branch.ComplementaryPair(a1, a2) => {
              (for ((arg1, arg2) <- a1.args zip a2.args) yield {
                (arg1, arg2)
              }).toList
            }
            case Branch.InvalidEquation(t1, t2) => {
              List((t1, t2))
            }
          }
        }


      // BREU arguments
      val breuDomains = this.order.toDomains()

      var nextDummyPredicate = 0
      val breuFlatEqs1 =
        (for (PositiveEquation(lhs, rhs) <- eqs) yield {
          nextDummyPredicate += 1
          val newPred = "dummy_predicate_" + nextDummyPredicate
          List((newPred, List(), lhs), (newPred, List(), rhs))
        }).flatten

      val breuFlatEqs2 =
        for (feq <- funEqs) yield {
          (feq.fun, feq.args, feq.res)
        }
      val breuGoals = goals

      Some((breuDomains, breuFlatEqs1 ++ breuFlatEqs2, breuGoals))
    }
  }



  def extend(pl : PseudoLiteral, augOrder : Order) = {
    val newOrder = order + augOrder    
    Branch(pl :: pseudoLiterals, newOrder, isClosed, strong, unitClauses)
  }

  lazy val regularityConstraints : List[DisunificationConstraint] = {
    val nonUnit = pseudoLiterals.take(pseudoLiterals.length - unitClauses)
    val h = nonUnit.head
    for (pl <- nonUnit.tail; if (h.lit.regularityConstraint(pl.lit).isDefined))
    yield h.lit.regularityConstraint(pl.lit).get
  }

  def instantiate(model : Model) = {
    // TODO: Maybe we can change the order here in a clever way?
    val newPseudoLiterals = pseudoLiterals.map(_.instantiate(model))
    Branch(newPseudoLiterals, order, isClosed, strong, unitClauses)
  }
}
