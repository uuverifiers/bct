package bct

import ap.parser._

object Term {
  def apply(iterm : ITerm) : Term = {
    Term(iterm.toString)
  }
}

case class Term(val term : String, val isUniversal : Boolean = false) {
  // forall/exists sign in front of universal/existential terms
  override def toString() = {
    if (isUniversal)
      8704.toChar.toString + term
    else
      8707.toChar.toString + term
  }
}
