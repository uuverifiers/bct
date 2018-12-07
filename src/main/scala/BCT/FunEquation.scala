package bct

case class FunEquation(val fun : String, val args : List[Term], val res : Term) {
  override def toString() = fun + "(" + args.mkString(",") + ") = " + res
}
