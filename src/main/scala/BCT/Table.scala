package bct

object Table {
  def apply(branches : List[Branch]) = {
    new Table(branches, List())
  }

  def apply(cls : PseudoClause) = {
    val branches = (for (l <- cls) yield Branch(List(l))).toList
    new Table(branches, List())
  }
}

class Table(openBranches : List[Branch], closedBranches : List[Branch], model : Model = Model.EmptyModel, blockingConstraints : BlockingConstraints = BlockingConstraints.Empty, strong : Boolean = true) {

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
  

  val length = openBranches.length + closedBranches.length
  val depth = if (openBranches.isEmpty) 0 else openBranches.map(_.depth).max
  val isClosed = openBranches.length == 0
  lazy val nextBranch = openBranches.head

  def closeBranches(branches : List[Branch]) = {
    Branch.tryClose(branches, blockingConstraints)
  }

  def close() : Option[Table] = {
    val branch = nextBranch.weak
    closeBranches(branch :: closedBranches) match {
      case None => None
      case Some((newModel, blockingConstraints)) => {
        val closedTable = new Table(openBranches.tail, branch.closed :: closedBranches, newModel, blockingConstraints)
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
      case Some((newModel, blockingConstraints)) => {
        val closedTable =
          new Table(restBranches ++ openBranches.tail, testBranch.closed :: closedBranches, newModel, blockingConstraints)
        Some(closedTable)
      }
    }
  }
}
