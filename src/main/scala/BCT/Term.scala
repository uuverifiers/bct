package bct

import ap.parser._

object Term {
  def apply(iterm : ITerm, id : Int) : Term = {
    Term(iterm.toString, id)
  }

  def apply(term : String, isUniversal : Boolean) : Term = {
    Term(term, 0, isUniversal)
  }
}

// TODO: Remove default id
case class Term(val term : String, val id : Int = 0, val isUniversal : Boolean = false, val isSkolem : Boolean = false) {
  // forall/exists sign in front of universal/existential terms
  override def toString() = {
    if (isUniversal)
      8704.toChar.toString + term
    else
      8707.toChar.toString + term
  }

  def copy(suffix : String) =
    if (isUniversal)
      Term(term + "$" + suffix, id, isUniversal)
    else if (isSkolem)
      Term(term + "$" + suffix, id, isUniversal, isSkolem)
    else
      this

  def instantiate(model : Model) = {
    if (isUniversal && (model contains this))
      model(this)
    else
      this
  }
}
