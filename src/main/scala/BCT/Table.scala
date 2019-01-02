package bct

/*
 A table consists of Branches

 When we extend by using PseudoClause, we split it into PseudoLiterals (one for each branch)

 */

object Table {
  def apply(branches : List[Branch]) = {
    new Table(branches, List())
  }

  def apply(cls : PseudoClause, strong : Boolean = true) = {
    create(cls, List(), strong)
  }

  def create(cls : PseudoClause, unitClauses : List[PseudoLiteral], strong : Boolean = true) = {
    val branches = (for (l <- cls.toPseudoLiterals()) yield {
      Branch(l :: unitClauses, cls.order, strong)
    }).toList
    new Table(branches, List())(strong)
  }
}

class Table(
  openBranches : List[Branch],
  closedBranches : List[Branch],
  val steps : List[(Int, Int)] = List(),
  val partialModel : Model = Model.EmptyModel,
  val fullModel : Model = Model.EmptyModel,
  blockingConstraints : BlockingConstraints = BlockingConstraints.Empty)
  (implicit strong : Boolean = true) {

  override def toString =
    "\ttable\n" + 
    (if (!openBranches.isEmpty)
      "----open----\n" + openBranches.mkString("\n") + "\n"
    else
      "") +
    (if (!closedBranches.isEmpty)
      "---closed---\n" + closedBranches.mkString("\n") + "\n"
    else
      "") + "\n"
  // "steps: " + steps.reverse.mkString(".") + "\n" +
  // "Model: \n" + partialModel

  

  val length = openBranches.length + closedBranches.length
  val depth = if (openBranches.isEmpty) 0 else openBranches.map(_.depth).max
  val isClosed = openBranches.length == 0
  lazy val nextBranch = openBranches.head

  def close(maxTime : Long) : Option[Table] =
    extendAndClose(PseudoClause.EmptyPseudoClause, 0, (-1, -1), maxTime)

  def extendAndClose(
    clause : PseudoClause,
    idx : Int,
    step : (Int, Int),
    maxTime : Long) : Option[Table] = {
    val (tBranch, rBranches) =
      if (clause.isEmpty) {
        // throw new Exception("...")
        (nextBranch.weak, List())
      } else {
        val branch = nextBranch
        val newBranches =
          (for (pl <- clause.toPseudoLiterals()) yield branch.extend(pl, clause.order)).toList
        val testBranch = newBranches(idx)
        val restBranches = newBranches.take(idx) ++ newBranches.drop(idx+1)
        (testBranch, restBranches)
      }

    // Extract regularity constraints
    // I.e., ipf the newly added head of a branch is similar in structure to a previous,
    // at least one of the literals must differ (i.e. a negative blocking clause)
    // val regularityConstraints : BlockingConstraints =
    //   BlockingConstraints((for (b <- restBranches) yield b.regularityConstraints).flatten)
    // val regularityConstraints :
    //     BlockingConstraints =  BlockingConstraints(testBranch.regularityConstraints)
    val regularityConstraints = BlockingConstraints(List())

    // val regularityConstraints =
    //   (for (b <- newBranches) yield {
    //     b.regularityConstraints
    //   }).fold(BlockingConstraints.Empty)(_ ++ _)

    Branch.tryClose(
      tBranch,
      closedBranches,
      blockingConstraints ++ regularityConstraints,
      maxTime) match {
      case None => None
      case Some((relModel, newFullModel, blockingConstraints)) => {
        // TODO: What if model extension doesn't work
        val newPartialModel = partialModel.extend(relModel).get        
        val closedTable =
          new Table(
            rBranches ++ openBranches.tail,
            tBranch.closed :: closedBranches,
            step :: steps,
            newPartialModel,
            newFullModel,
            blockingConstraints)
        Some(closedTable.instantiate(newPartialModel))
      }
    }
  }


  def instantiate(model : Model) = {
    val newOpenBranches = openBranches.map(_.instantiate(model))
    val newClosedBranches = closedBranches.map(_.instantiate(model))
    new Table(
      newOpenBranches,
      newClosedBranches,
      steps,
      partialModel,
      fullModel,
      blockingConstraints)(strong)
  }
}
