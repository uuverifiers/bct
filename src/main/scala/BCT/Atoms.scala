package bct

object Atom {
  def apply(predicate : String, args : List[String]) = {
    new Atom(predicate, args)
  }

}

class Atom(predicate : String, args : List[String]) {
  override def toString() = predicate + "(" + args.mkString(",") + ")"
}


object FunEquation {
  def apply(fun : String, args : List[String], res : String) = {
    new FunEquation(fun, args, res)
  }
}

class FunEquation(fun : String, args : List[String], res : String) {
  override def toString() = fun + "(" + args.mkString(",") + ") = " + res
}
