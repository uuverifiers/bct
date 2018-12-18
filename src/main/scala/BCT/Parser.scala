package bct

import scala.collection.mutable.ListBuffer
import ap.parser.{Environment, TPTPTParser}
import ap.parameters.{Param, ParserSettings}
import ap.parser._

object Parser {

  // Representing terms from quantifiers/ since they have to be flat
  // skolemized existential constants will need a funequation
  type QPair = (Term, List[FunEquation])

  var allCount = -1
  var exCount = -1
  var skolemCount = -1
  var id = -1

  def newEx(quantifiers : List[QPair] = List()) : QPair = {
    id += 1
    if (quantifiers.exists(_._1.isUniversal)) {
      skolemCount += 1
      val res = Term("s_" + skolemCount, id, false, true)
      val skolemFunction = "skolem_" + (for ((q, _) <- quantifiers) yield q.term).mkString(".")

      // TODO: We should sort the quantifiers... ? Only universal?
      val funEq = FunEquation(skolemFunction, quantifiers.map(_._1), res)
      (res, List(funEq))
    } else {
      exCount += 1
      (Term("c_" + exCount, id, false), List())
    }
  }

  def newAll() : QPair = {
    allCount += 1
    id += 1
    (Term("x_" + allCount, id, true), List())
  }

  def fixTerm(t : ITerm, quantifiers : List[QPair]) : (Term, List[FunEquation]) = {
    t match {
      case IVariable(index) => quantifiers(index)

      // TODO: Maybe ID can not be 0...
      case IConstant(c) => (Term(c.toString, 0), List())
      case IFunApp(fun, args) => {
        val funEqs = ListBuffer() : ListBuffer[FunEquation]
        val newArgs =
          for (a <- args) yield {
            val (newArg, newFunEqs) = fixTerm(a, quantifiers)
            funEqs ++= newFunEqs
            newArg
          }

        // TODO: Should this be affected by quantifiers (i.e., do we need to skolemize)
        val (newRes, _) = newEx()
        funEqs += FunEquation(fun.toString, newArgs.toList, newRes)
        (newRes, funEqs.toList)
      }
    }
  }

  def atom2Internal(pred : ap.terfor.preds.Predicate, args : Seq[ITerm], quantifiers : List[QPair], negated : Boolean = true) : PseudoLiteral = {
    val allEqs = ListBuffer() : ListBuffer[FunEquation]
    val newArgs =
      for (a <- args) yield {
        val (term, funEqs) = fixTerm(a, quantifiers)
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

  def eq2Internal(term : ITerm, quantifiers : List[QPair], negated : Boolean = true) : PseudoLiteral = {
    val (t1, t2) =
      term match {
        case IPlus(t1, ITimes(ap.basetypes.IdealInt(-1), t2)) => (t1, t2)
        case IPlus(ITimes(ap.basetypes.IdealInt(-1), t1), t2) => (t1, t2)
      }

    val (nt1, feq1) = fixTerm(t1, quantifiers)
    val (nt2, feq2) = fixTerm(t2, quantifiers)

    if (negated)
      PseudoLiteral(feq1 ++ feq2, NegativeEquation(nt1, nt2))
    else
      PseudoLiteral(feq1 ++ feq2, PositiveEquation(nt1, nt2))
  }


  def toCNF(formula : IFormula, quantifiers : List[QPair]) : List[List[PseudoLiteral]] = {
    val ret = 
      formula match {
        // TODO: Check negations
        case INot(INot(sf)) => toCNF(sf, quantifiers)

        case IBoolLit(true) => List(List(PseudoLiteral(True)))
        case IBoolLit(false) => List(List(PseudoLiteral(True)))

        case IAtom(pred, args) => List(List(atom2Internal(pred, args, quantifiers, false)))
        case INot(IAtom(pred, args)) => List(List(atom2Internal(pred, args, quantifiers, true)))

        case IIntFormula(IIntRelation.EqZero, t) => List(List(eq2Internal(t, quantifiers, false)))
        case INot(IIntFormula(IIntRelation.EqZero, t)) => List(List(eq2Internal(t, quantifiers, true)))



        case IQuantified(ap.terfor.conjunctions.Quantifier.ALL, sf) => toCNF(sf, newAll() :: quantifiers)
        case INot(IQuantified(ap.terfor.conjunctions.Quantifier.EX, sf)) => toCNF(~sf, newAll() :: quantifiers)

        case IQuantified(ap.terfor.conjunctions.Quantifier.EX, sf) => toCNF(sf, newEx(quantifiers) :: quantifiers)
        case INot(IQuantified(ap.terfor.conjunctions.Quantifier.ALL, sf)) => toCNF(~sf, newEx(quantifiers) :: quantifiers)


        

        case INamedPart(_, sf) => toCNF(sf, quantifiers)
        case INot(INamedPart(_, sf)) => toCNF(INot(sf), quantifiers)                    

        
        case INot(IBinFormula(IBinJunctor.Or, sf1, sf2)) => toCNF(~sf1, quantifiers) ++ toCNF(~sf2, quantifiers)
        case IBinFormula(IBinJunctor.And, sf1, sf2) => toCNF(sf1, quantifiers) ++ toCNF(sf2, quantifiers)


        case IBinFormula(IBinJunctor.Or, sf1, sf2) => {
          (for (c1 <- toCNF(sf1, quantifiers); c2 <- toCNF(sf2, quantifiers)) yield {
            c1 ++ c2
          }).toList
        }
        case INot(IBinFormula(IBinJunctor.And, sf1, sf2)) => {
          (for (c1 <- toCNF(~sf1, quantifiers); c2 <- toCNF(~sf2, quantifiers)) yield {
            c1 ++ c2
          }).toList
        }


        case INot(IBinFormula(IBinJunctor.Eqv, sf1, sf2)) => toCNF(IBinFormula(IBinJunctor.Eqv, ~sf1, sf2), quantifiers)
        case IBinFormula(IBinJunctor.Eqv, sf1, sf2) => {
          val nsf1 = IBinFormula(IBinJunctor.Or,~sf1,sf2)
          val nsf2 = IBinFormula(IBinJunctor.Or,~sf2,sf1)
          toCNF(nsf1, quantifiers) ++ toCNF(nsf2, quantifiers)
        }
      }
    // println("toCNF(" + formula + ") ---> " + ret)
    ret
  }


  def tptp2Internal(fileName : String) : Option[List[PseudoClause]]= {
    allCount = -1
    exCount = -1
    skolemCount = -1
    try {    
      if (fileName contains "+") {
        val reader = new java.io.BufferedReader (
          new java.io.FileReader(new java.io.File (fileName)))
        val settings = Param.BOOLEAN_FUNCTIONS_AS_PREDICATES.set(ParserSettings.DEFAULT, true)
        val (formula, list, signature) = new TPTPTParser(new Environment, settings)(reader)
        val CNF = toCNF(~formula, List())
        Some(CNF.map(PseudoClause(_)))
      } else if (fileName contains "-") {
        // Already in CNF
        // val reader = new java.io.BufferedReader (
        //   new java.io.FileReader(new java.io.File (fileName)))
        // val settings = Param.BOOLEAN_FUNCTIONS_AS_PREDICATES.set(ParserSettings.DEFAULT, true)
        // val (formula, list, signature) = new TPTPTParser(new Environment, settings)(reader)
        // Some(formula2Internal(formula))
        println("CNF problems must be checked with skolem constants.")
        None
      } else {
        println("Only TPTP problems with '+' or '-' supported.")
        None
      }
    } catch {
      case e : java.io.FileNotFoundException =>  {
        println("File \"" + fileName + "\" not found.")
        None
      }
    }
  }
}
