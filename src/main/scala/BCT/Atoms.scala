package bct

object Atom {
  def apply(predicate : String, args : List[String]) = {
    new Atom(predicate, args)
  }

}

class Atom(val predicate : String, args : List[String]) {
  override def toString() = predicate + "(" + args.mkString(",") + ")"
}
