package bct

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

class Table(openBranches : List[Branch], closedBranches : List[Branch], model : Model = Model.EmptyModel, strong : Boolean = true) {

  // override def toString() = "<<<TABLE>>>\n" + openBranches.mkString("\n") + "\n--closed--\n" + closedBranches.mkString("\n") + "\n<<</TABLE>>>"
  override def toString() =
    "<<<TABLE>>>\n" +
  openBranches.mkString("\n") + "\n" +
  // model + 
  "\n<<</TABLE>>>"

  def fullString() =
    "<<<TABLE>>>\n" +
  openBranches.mkString("\n") + "\n" + 
  "<--------->\n" +
  closedBranches.mkString("\n") + "\n" +
  // model +  
  "\n<<</TABLE>>>"    
  

  def length = openBranches.length + closedBranches.length
  def depth = openBranches.map(_.depth).max
  def isClosed = openBranches.length == 0
  def nextBranch = openBranches.head


  def closeBranches(branches : List[Branch]) = {
    val retryModel = Branch.tryClose(branches, model)
    if (retryModel.isDefined)
      retryModel
    else
      Branch.tryClose(branches)
  }

  def close() : Option[Table] = {
    val branch = nextBranch.weak
    closeBranches(branch :: closedBranches) match {
      case None => None
      case Some(newModel) => {
        val closedTable = new Table(openBranches.tail, branch.closed :: closedBranches, newModel)
        Some(closedTable)
      }
    }
  }

  def extendAndClose(clause : PseudoClause, idx : Int) : Option[Table] = {
    val branch = nextBranch
    val newBranches = (for (pl <- clause) yield branch.extend(pl)).toList
    val testBranch = newBranches(idx)
    val restBranches = newBranches.take(idx) ++ newBranches.drop(idx+1)
    closeBranches(testBranch :: closedBranches) match {    
      case None => None
      case Some(newModel) => {
        val closedTable =
          new Table(restBranches ++ openBranches.tail, testBranch.closed :: closedBranches, newModel)
        Some(closedTable)
      }
    }
  }
}
