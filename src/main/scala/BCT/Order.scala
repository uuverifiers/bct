package bct

import scala.collection.immutable.{Set, Map}

// The order is stored as ..., (t2, univ_t_2), (t1_, univ_t_1), where t1 is
// the smallest term and univ_t_1 means that t_1 is universal
// (i.e. can take the value of all smaller or equal terms.
// Smallest term is last in list

object Order {
  def combineOrders(orders : List[Order]) : Order = {
    var tmpOrder = orders.head
    for (ord <- orders.tail)
      tmpOrder = tmpOrder + ord

    tmpOrder
  }

  val MIN_TERM = Term(9775.toChar.toString, false)
}


case class Order(val terms_ : List[(Term)]) {
  // TODO: Here we are inserting dummy-element!
  val terms = 
    if (terms_.last.isUniversal)
      terms_ ++ List(Order.MIN_TERM)
    else
      terms_

  override def toString() = {
    terms.mkString(">")
  }


  def +(that : Order) = {
    // Universal variables should not exists twice!
    assert(that.terms.filter(_.isUniversal).forall(t => !(terms contains t)))
    val newConstants = that.terms_.filter(!_.isUniversal)
    val newVariables = that.terms_.filter(_.isUniversal)
    Order(newVariables ++ newConstants.filterNot(terms_ contains _) ++ terms_)
  }

  def toDomains(model : Model = Model.EmptyModel) : Domains = {
    import scala.collection.mutable.{Set => MSet, Map => MMap}

    val seenTerms = MSet() : MSet[Term]
    val domains = MMap() : MMap[Term, Set[Term]]
    for (t <- terms.reverse) {
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
