package bct

case class FunEquation(val fun : String, val args : List[Term], val res : Term) {
  override def toString() = fun + "(" + args.mkString(",") + ") = " + res
  def copy(suffix : String) = FunEquation(fun, args.map(_.copy(suffix)), res.copy(suffix))

  def instantiate(model : Model) = {
    val newArgs = args.map(_.instantiate(model))
    val newRes = res.instantiate(model)
    FunEquation(fun, newArgs, newRes)
  }
}
