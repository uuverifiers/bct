package bct

object Atom {
  def apply(predicate : String, args : List[Term]) = {
    new Atom(predicate, args)
  }

}

class Atom(val predicate : String, val args : List[Term]) {
  override def toString() = predicate + "(" + args.mkString(",") + ")"
  val terms = args
}
