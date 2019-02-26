package bct

import ap.parser._

object Term {
  def apply(term : String, id : Int) : Term =
    new Term(term, id)  
  def apply(term : String, id : Int, isUniversal : Boolean) : Term =
    new Term(term, id, isUniversal)
  def apply(term : String, id : Int, isUniversal : Boolean, isSkolem : Boolean) =
    new Term(term, id, isUniversal, isSkolem)


  def apply(term : String, isUniversal : Boolean) : Term =
    new Term(term, 0, isUniversal)    


  def apply(iterm : ITerm, id : Int) : Term = {
    new Term(iterm.toString, id)
  }  
}

// TODO: Evaluate the use of ID and see if it is necessary?

class Term(val term : String, val id : Int, val isUniversal : Boolean = false, val isSkolem : Boolean = false) {
    def canEqual(a: Any) = a.isInstanceOf[Term]

  override def equals(that: Any): Boolean =
    that match {
      case that : Term => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }

  override def hashCode: Int = term.hashCode

  override def toString() = {
    (if (isUniversal)
      8704.toChar.toString + term
    else
      8707.toChar.toString + term) // + "[" + id + "]"
  }

  def copy(suffix : String) =
    if (isUniversal)
      new Term(term + "$" + suffix, id, isUniversal)
    else if (isSkolem)
      new Term(term + "$" + suffix, id, isUniversal, isSkolem)
    else
      this

  def instantiate(model : Model) = {
    if (isUniversal && (model contains this))
      model(this)
    else
      this
  }
}
