package bct

// import scala.collection.mutable.PriorityQueue

object Table {
  def apply(branches : List[Branch]) = {
    // implicit val bOrd = BranchOrdering
    // val pQ = PriorityQueue[Branch]()
    // for (b <- branches)
    //   pQ.enqueue(b)
    new Table(branches, List())
  }

  def apply(cls : PseudoClause) = {
    // implicit val bOrd = BranchOrdering
    // val pQ = PriorityQueue[Branch]()
    // for (l <- cls) 
    //   pQ.enqueue(Branch(List(l)))
    val branches = (for (l <- cls) yield Branch(List(l))).toList
    new Table(branches, List())
  }
}

// class Table(openBranches : List[Branch], closedBranches : List[Branch])(implicit branchOrdering : Ordering[Branch]) {
class Table(openBranches : List[Branch], closedBranches : List[Branch], strong : Boolean = true) {

  override def toString() = "<<<TABLE>>>\n" + openBranches.mkString("\n") + "\n--closed--\n" + closedBranches.mkString("\n") + "\n<<</TABLE>>>"

  def length = openBranches.length + closedBranches.length
  def isClosed = openBranches.length == 0
  def nextBranch = openBranches.head


  def close() : Option[Table] = {
    val branch = nextBranch
    nextBranch.tryClose() match {
      case None => None
      case Some(closedBranch) => {
        // We need to add some substitution propagation here
        val closedTable = new Table(openBranches.tail, closedBranch :: closedBranches)
        Some(closedTable)
      }
    }
  }

  def extendAndClose(clause : PseudoClause, idx : Int) : Option[Table] = {
    val branch = nextBranch
    val newBranches = (for (pl <- clause) yield branch.extend(pl)).toList
    val testBranch = newBranches(idx)
    val restBranches = newBranches.take(idx) ++ newBranches.drop(idx+1)
    // TODO: Fix Strong Connections
    testBranch.tryClose() match {
      case None => None
      case Some(closedBranch) => {
        val closedTable = new Table(restBranches ++ openBranches.tail, closedBranch :: closedBranches)
        Some(closedTable)
      }
    }
  }
}
