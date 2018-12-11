package bct

import scala.collection.mutable.ListBuffer
import ap.parser.{Environment, TPTPTParser}
import ap.parameters.{Param, ParserSettings}
import ap.parser._

object Parser {

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

  def fixTerm(t : ITerm, quantifiers : List[Term]) : (List[FunEquation], Term) = {
    t match {
      case IVariable(index) => (List(), quantifiers(index))
      case IConstant(c) => (List(), Term(c.toString))
      case IFunApp(fun, args) => {
        val funEqs = ListBuffer() : ListBuffer[FunEquation]
        val newArgs =
          for (a <- args) yield {
            val (newFunEqs, newArg) = fixTerm(a, quantifiers)
            funEqs ++= newFunEqs
            newArg
          }
        val newRes = newEx()
        funEqs += FunEquation(fun.toString, newArgs.toList, newRes)
        (funEqs.toList, newRes)
      }
    }
  }

  def atom2Internal(pred : ap.terfor.preds.Predicate, args : Seq[ITerm], quantifiers : List[Term], negated : Boolean = false) : PseudoLiteral = {
    val allEqs = ListBuffer() : ListBuffer[FunEquation]
    val newArgs =
      for (a <- args) yield {
        val (funEqs, term) = fixTerm(a, quantifiers)
        allEqs ++= funEqs
        term
      }

    val atom = Atom(pred.toString, newArgs.toList)
    val lit =
      if (negated)
        NegativeLiteral(atom)
      else
        PositiveLiteral(atom)

    PseudoLiteral(allEqs.toList, lit)
  }

  def eq2Internal(term : ITerm, quantifiers : List[Term], negated : Boolean = false) : PseudoLiteral = {
    val (t1, t2) =
      term match {
        case IPlus(t1, ITimes(ap.basetypes.IdealInt(-1), t2)) => (t1, t2)
        case IPlus(ITimes(ap.basetypes.IdealInt(-1), t1), t2) => (t1, t2)
      }

    val (feq1, nt1) = fixTerm(t1, quantifiers)
    val (feq2, nt2) = fixTerm(t1, quantifiers)

    if (negated)
      PseudoLiteral(feq1 ++ feq2, NegativeEquation(nt1, nt2))
    else
      PseudoLiteral(feq1 ++ feq2, PositiveEquation(nt1, nt2))
  }

  def literals2Internal(formula : IFormula, quantifiers : List[Term]) : List[PseudoLiteral] = {
    formula match {
      case IBoolLit(true) => List(PseudoLiteral(True))
      case IBoolLit(false) => List(PseudoLiteral(False))

      case IAtom(pred, args) => List(atom2Internal(pred, args, quantifiers))
      case INot(IAtom(pred, args)) => List(atom2Internal(pred, args, quantifiers, true))

      case IIntFormula(IIntRelation.EqZero, t) => {
        List(eq2Internal(t, quantifiers))
      }
      case INot(IIntFormula(IIntRelation.EqZero, t)) => {
        List(eq2Internal(t, quantifiers, true))
      }

      case IBinFormula(IBinJunctor.Or, f1, f2) => {
        val if1 = literals2Internal(f1, quantifiers)
        val if2 = literals2Internal(f2, quantifiers)
        if1 ++ if2
      }
    }
  }


  def countQuantifiers(formula : IFormula, count : Int = 0) : (IFormula, Int) = {
    formula match {
      case IQuantified(ap.terfor.conjunctions.Quantifier.ALL, subFormula) => countQuantifiers(subFormula, count+1)
      case formula => (formula, count)
    }
  }

  def conjunction2Internal(formula : IFormula) : PseudoClause = {
    val pseudoLiterals =
      formula match {
        case INamedPart(_, subFormula) => conjunction2Internal(subFormula)
        case INot(subFormula) => {
          val (subFormula2, quanCount) = countQuantifiers(subFormula)
          val quantifiers = (for (i <- 0 until quanCount) yield newAll()).toList
          literals2Internal(subFormula2, quantifiers)
        }
      }
    PseudoClause(pseudoLiterals.toList)
  }

  def formula2Internal(formula : IFormula, negated : Boolean = false) : List[PseudoClause] = {
    val pseudoClauses =
      formula match {
        case INamedPart(name, subFormula) => {
          println("Ignoring name (" + name + ")")
          formula2Internal(subFormula)
        }

        case INot(IBoolLit(true)) => List(PseudoClause(PseudoLiteral(False)))
        case INot(IBoolLit(false)) => List(PseudoClause(PseudoLiteral(True)))          


        case IBinFormula(IBinJunctor.Or, f1, f2) => {
          val if1 =
            if (f1.length == 1)
              List(conjunction2Internal(f1))
            else
              formula2Internal(f1)

          val if2 =
            if (f2.length == 1)
              List(conjunction2Internal(f2))
            else
              formula2Internal(f2)

          if1 ++ if2
        }
      }

    pseudoClauses
  }

  def tptp2Internal(fileName : String) = {
    try {
      val reader = new java.io.BufferedReader (
        new java.io.FileReader(new java.io.File (fileName)))

      val settings = Param.BOOLEAN_FUNCTIONS_AS_PREDICATES.set(ParserSettings.DEFAULT, true)

      val (formula, list, signature) = new TPTPTParser(new Environment, settings)(reader)

      allCount = -1
      exCount = -1
      Some(formula2Internal(formula))
    } catch {
      case e : java.io.FileNotFoundException =>  None
    }
  }
}
