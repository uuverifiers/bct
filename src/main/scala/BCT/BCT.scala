package bct

import scala.collection.mutable.ListBuffer


object BCT extends App {


  val inputBuffer = ListBuffer() : ListBuffer[String]

  //
  // OPTIONS
  //

  def handleArguments(args : List[String]) : Unit = {
    val timeoutR = "-(timeout|to)=(\\d+)".r
    val regularityR = "(\\+|-)regularity".r
    val pruneModelR = "(\\+|-)prune".r
    val instantiateR = "(\\+|-)instantiate".r        
    val progressPrintR = "(\\+|-)progress".r    
    val startClauseR = "-start-clause=(\\d+)".r
    val debugR = "(\\+|-)debug".r

    if (!args.isEmpty) {
      args.head match {
        case timeoutR(_, time) => Settings.timeout = time.toInt

        case regularityR("+") => Settings.regularity = true
        case regularityR("-") => Settings.regularity = false

        case instantiateR("+") => Settings.instantiate = true
        case instantiateR("-") => Settings.instantiate = false          

        case pruneModelR("+") => Settings.prune_model = true
        case pruneModelR("-") => Settings.prune_model = false          

        case progressPrintR("+") => Settings.progress_print = true
        case progressPrintR("-") => Settings.progress_print = false          

        case debugR("+") => Settings.debug = true
        case debugR("-") => Settings.debug = false          

        case startClauseR(idx) => Settings.start_clause = Some(idx.toInt)

        case str => inputBuffer += str
      }
      handleArguments(args.tail)
    }
  }

  handleArguments(args.toList)

  val inputs = inputBuffer.toList.reverse  

  Settings.print()

  println("INPUTS:")
  for (i <- inputs)
    println("\t" + i)

  for (i <- inputs) {
    println(Benchmarker.run(i))
  }


  //   args(0) match {
  //     case "dir" => {
  //       val inputDir = args(1)
  //       val split = args(1).split('/').reverse
  //       val outFile = 
  //         (if (split(0) == "")
  //           split(1)
  //         else
  //           split(0)) + ".out"
  //       Benchmarker.testDir(inputDir, outFile, TIMEOUT)
  //     }
  //     case "parse" => {
  //       Benchmarker.parseFile(args(1))
  //     }
  //   }
  // }
}

