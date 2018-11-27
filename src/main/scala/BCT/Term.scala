package bct

object Term {
  def apply(term : String) = {
    new Term(term)
  }

}

class Term(term : String) {
  override def toString() = term
}
