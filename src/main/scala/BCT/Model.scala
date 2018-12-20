package bct


object Model {
  def apply(assignments : Map[Term, Term]) = {
    new Model(assignments)
  }

  val EmptyModel = Model(Map())
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
    if (newModel.assignments.exists{
      case (k,v) => assignments.contains(k) && assignments(k) != v
    }) {
      None
    } else {
      Some(Model(assignments ++ newModel.assignments))
    }
  }
}



