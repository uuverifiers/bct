package bct

object FunEquation {
  def apply(fun : String, args : List[Term], res : Term) = {
    new FunEquation(fun, args, res)
  }
}

class FunEquation(val fun : String, val args : List[Term], val res : Term) {
  override def toString() = fun + "(" + args.mkString(",") + ") = " + res
}
