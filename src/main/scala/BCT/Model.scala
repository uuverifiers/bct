package bct


object Model {
  def apply(assignments : Map[Term, Term]) = {
    new Model(assignments)
  }

  val Empty = Model(Map())
}

class Model(val assignments : Map[Term, Term]) {

  override def toString() = {
    assignments.mkString("\n")
  }

  def apply(t : Term) = assignments(t)
  def contains(t : Term) = assignments.contains(t)

  def removeMin() : Model = {
    Model(assignments.filter{ case (_, v) => v != Order.MIN_TERM})
  }

  def extend(newModel : Model) : Option[Model] = {
    // Check that there are no conflicts
    if (newModel.assignments.exists{case (k,v) => assignments.contains(k) && assignments(k) != v && assignments(k) != k}) {
      println("Old model")
      println(this)
      println("Extending model")
      println(newModel)
      throw new Exception("Trying to extend with disagreeing model")
      None
    } else {
      Some(Model(assignments ++ newModel.assignments))
    }
  }
}



