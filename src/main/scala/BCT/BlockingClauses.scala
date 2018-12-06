package bct

object BlockingClauses {
  val EmptyBlockingClauses  = BlockingClauses(List())  
}

case class BlockingClauses(val blockingClauses : List[List[(Term, Term)]]) {

}



