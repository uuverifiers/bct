package bct

object Term {
  def apply(term : String, isUniversal : Boolean = false) = {
    new Term(term, isUniversal)
  }

}

class Term(val term : String, val isUniversal : Boolean) {
  override def toString() = {
    if (isUniversal)
      8704.toChar.toString + term
    else
      8707.toChar.toString + term
  }

  override def equals(that : Any) = {
    if (that.isInstanceOf[Term])
      term == that.asInstanceOf[Term].term
    else
      false
  }

  override def hashCode = term.hashCode
}
