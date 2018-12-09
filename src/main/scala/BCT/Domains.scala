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
        newMap += k -> (domains(k).intersect(that.domains(k)))
      else
        newMap += k -> v
    }

    Domains(newMap.toMap)
  }
}
