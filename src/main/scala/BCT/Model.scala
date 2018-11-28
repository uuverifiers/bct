package bct


object Model {
  def apply(assignments : Map[Term, Term]) = {
    new Model(assignments)
  }

  val EmptyModel = Model(Map())
}

class Model(val assignemnts : Map[Term, Term]) {
}



