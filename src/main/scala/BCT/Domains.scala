package bct

object Domains {
  def apply(domains : Map[Term, Set[Term]]) = {
    new Domains(domains)
  }

  val EmptyDomains = new Domains(Map())
}


class Domains(val domains : Map[Term, Set[Term]]) {

  override def toString() = domains.toString()

  def extend(that : Domains) = {
    import scala.collection.mutable.{Map => MMap}
    val newMap = MMap() ++ domains
    for ((k, v) <- that.domains) {
      if (newMap contains k)
        newMap += k -> (domains(k).intersect(that.domains(k)))
      else
        newMap += k -> v
    }

    Domains(newMap.toMap)
  }

  def assign(model : Model) = {
    val newDomains = 
      for ((k, v) <- domains) yield {
        if (model.contains(k))
          k -> Set(model(k))
        else
          k -> v
      }
    Domains(newDomains)
  }
}
