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
// TODO: Maybe have sub-classes for strong and weak table?
class Table(openBranches : List[Branch], closedBranches : List[Branch], model : Model = Model.EmptyModel, strong : Boolean = true) {

  // override def toString() = "<<<TABLE>>>\n" + openBranches.mkString("\n") + "\n--closed--\n" + closedBranches.mkString("\n") + "\n<<</TABLE>>>"
  override def toString() = "<<<TABLE>>>\n" + openBranches.mkString("\n") + "\n<<</TABLE>>>"  

  def length = openBranches.length + closedBranches.length
  def isClosed = openBranches.length == 0
  def nextBranch = openBranches.head


  def close() : Option[Table] = {
    val branch = nextBranch
    nextBranch.tryClose() match {
      case None => None
      case Some((closedBranch, model)) => {
        // TODO :Combine models...
        val closedTable = new Table(openBranches.tail, closedBranch :: closedBranches, model)
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
    // TODO :Combine models...
    testBranch.tryClose() match {
      case None => None
      case Some((closedBranch, model)) => {
        val closedTable = new Table(restBranches ++ openBranches.tail, closedBranch :: closedBranches, model)
        Some(closedTable)
      }
    }
  }
}
