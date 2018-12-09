package bct

import scala.collection.immutable.{Set, Map}

// The order is stored as (t1, univ_t_1), (t2_, univ_t_2), where t1 is
// the smallest term and univ_t_1 means that t_1 is universal
// (i.e. can take the value of all smaller or equal terms.


case class Order(val terms_ : List[(Term)]) {
  // TODO: Here we are inserting dummy-element!
  val terms = Term(9775.toChar.toString, false) :: terms_

  override def toString() = {
    terms.mkString("<")
  }

  def toDomains(model : Model = Model.EmptyModel) : Domains = {
    import scala.collection.mutable.{Set => MSet, Map => MMap}

    val seenTerms = MSet() : MSet[Term]
    val domains = MMap() : MMap[Term, Set[Term]]
    for (t <- terms) {
      seenTerms += t
      if (!t.isUniversal)
        domains += (t -> Set(t))
      else if (model.contains(t))
        domains += (t -> Set(model(t)))
      else 
        domains += (t -> seenTerms.toSet)
    }

    Domains(domains.toMap)
  }
}
