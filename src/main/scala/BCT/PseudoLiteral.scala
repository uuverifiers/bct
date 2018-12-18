package bct

import scala.collection.mutable.{Set => MSet}

object PseudoLiteral {
  def apply(lit_ : Literal) : PseudoLiteral =
    PseudoLiteral(List(), lit_)
}

case class PseudoLiteral (val funEquations : List[FunEquation], val lit : Literal)  {
  override def toString = {
    if (funEquations.isEmpty)
      "{" + lit + "}"
    else
    "{" + funEquations.mkString("^") + "::" + lit + "}"
  }

  // Only used if sort is known!
  lazy val atom : Atom = {
    lit match {
      case PositiveLiteral(a) => a
      case NegativeLiteral(a) => a
      case _ => throw new Exception("Retrieving atom from " + lit)
    }
  }

  // Only used if sort is known!  
  lazy val equation : Equation = {
    lit match {
      case lit : NegativeEquation => lit
      case lit : PositiveEquation => lit
      case _ => throw new Exception("Retrieving equation from " + lit)        
    }
  }

  val terms : Set[Term] = {
    val allTerms = MSet() ++ lit.terms : MSet[Term]
    for (FunEquation(f, args, res) <- funEquations) {
      allTerms += res
      for (a <- args)
        allTerms += a
    }
    allTerms.toSet
  }

  lazy val order = {
    Order(terms.toList.sortBy(-_.id))
  }

  def isComplementary(that : PseudoLiteral) =
    lit.isComplementary(that.lit)

  def copy(suffix : String) = PseudoLiteral(for (feq <- funEquations) yield feq.copy(suffix), lit.copy(suffix))

  def instantiate(model : Model) = {
    val newFunEquations = funEquations.map(_.instantiate(model))
    val newLit = lit.instantiate(model)
    PseudoLiteral(newFunEquations, newLit)
  }
}
