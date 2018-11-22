package bct

object Branch {
  def apply(pseudoLiterals : List[PseudoLiteral]) = {
    new Branch(pseudoLiterals, Open)
  }

  class Conflict
  case object Open extends Conflict
  case class ComplementaryPair(i1 : Int , i2 : Int) extends Conflict
  case class InvalidEquation(i : Int) extends Conflict
}


object BranchOrdering extends Ordering[Branch] {
  def compare(x : Branch, y : Branch) = {
    x.length.compare(y.length)
  }
}



class Branch(pseudoLiterals : List[PseudoLiteral], closed : Branch.Conflict) {
  def length = pseudoLiterals.length

  override def toString() = pseudoLiterals.mkString("->")

  def close(conflict : Branch.Conflict) =
    new Branch(pseudoLiterals, conflict)
}
