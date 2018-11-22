package bct

class Literal {}

case class PositiveLiteral(atom : Atom) extends Literal {
  override def toString() = atom.toString()
}
case class NegativeLiteral(atom : Atom) extends Literal {
  override def toString() = "(-" + atom + ")"
}

case class PositiveFlatEquation(lhs : String, rhs : String) {
  override def toString() = lhs + " = " + rhs
}

case class NegativeFlatEquation(lhs : String, rhs : String) {
  override def toString() = lhs + " != " + rhs
}
