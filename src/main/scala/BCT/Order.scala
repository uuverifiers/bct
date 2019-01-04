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

  val Empty = Order(List())
  val MIN_TERM = Term(9775.toChar.toString, false)
}


case class Order(terms : List[(Term)]) {
  // TODO: Here we are inserting dummy-element!
  // val terms = 
  //   if (terms_.isEmpty)
  //     List(Order.MIN_TERM)
  //   else if (terms_.last.isUniversal)
  //     terms_ ++ List(Order.MIN_TERM)
  //   else
  //     terms_

  override def toString() = {
    terms.mkString(">")
  }


  def +(that : Order) = {

    // assert(that.terms.filter(_.isUniversal).forall(t => !(terms contains t)))

    // val newVariables = that.terms_.filter(_.isUniversal)

    // Given two orders ... All universals should be pulled to the
    // front (if they existed before) while constans should be left
    // remaining

    val newConstants = that.terms.filter(!_.isUniversal)
    val oldTerms = this.terms.filterNot(t => t.isUniversal && (that.terms contains t))
    val newTerms = that.terms.filterNot(t => !t.isUniversal && (this.terms contains t))

    Order(newTerms ++ oldTerms)
  }

  def toDomains(model : Model = Model.Empty) : Domains = {
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

  // Adds to the bottom of the order
  def addConstants(constants : List[Term]) = {
    Order(terms ++ constants.filterNot(terms contains _))
  }

  def copy(suffix : String) = {
    Order(terms.map(_.copy(suffix)))
  }
}
