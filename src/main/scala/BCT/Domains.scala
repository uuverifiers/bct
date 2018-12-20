package bct

object Domains {
  def apply(domains : Map[Term, Set[Term]]) = {
    new Domains(domains)
  }

  val Empty = new Domains(Map())
}


case class Domains(val domains : Map[Term, Set[Term]]) {

  override def toString() = domains.toString()

  def apply(t : Term) = domains(t)

  def extend(that : Domains) = {
    import scala.collection.mutable.{Map => MMap}
    val newMap = MMap() ++ domains
    for ((k, v) <- that.domains) {
      if (newMap contains k)
        // TODO: Is this always sound?
        newMap += k -> (domains(k).union(that.domains(k)))
      else
        newMap += k -> v
    }

    Domains(newMap.toMap)
  }

  def fix() = {
    if (domains.exists{ case (t, d) => t.isUniversal && d == Set(t) }) {
      val minTerm = Order.MIN_TERM
      val map = 
        for ((k, v) <- domains) yield {
          if (k.isUniversal)
            k -> (v + minTerm)
          else
            k -> v
        }
      Domains(map)
    } else {
      this
    }
  }
}
