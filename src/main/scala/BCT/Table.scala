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
class Table(openBranches : List[Branch], closedBranches : List[Branch]) {

  override def toString() = "<<<TABLE>>>\n" + openBranches.mkString("\n") + "\n--closed--\n" + closedBranches.mkString("\n") + "\n<<</TABLE>>>"

  def length = openBranches.length + closedBranches.length
  def isClosed = openBranches.length == 0
  def nextBranch = openBranches.head

  def close(conflict : Branch.Conflict) = {
    val newClosedBranches = openBranches.head.close(conflict) :: closedBranches
    val newOpenBranches = openBranches.tail
    new Table(newOpenBranches, newClosedBranches)
  }
}
