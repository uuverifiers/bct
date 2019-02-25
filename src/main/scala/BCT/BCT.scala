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
    val essentialR = "(\\+|-)essential".r    
    val startMaxDepthR = "-start-max-depth=(\\d+)".r
    val progressPrintR = "(\\+|-)progress".r
    val fullTablePrintR = "(\\+|-)full-table".r        
    val startClauseR = "-start-clause=(\\d+)".r
    val hardCodedR = "-hard-coded=([-]?\\d+\\.\\d+[;[-]?\\d+\\.\\d+]*)".r    
    val debugR = "(\\+|-)debug".r
    val timeR = "(\\+|-)time".r    
    val saveBreuR = "(\\+|-)save-breu".r    

    if (!args.isEmpty) {
      args.head match {
        case timeoutR(_, time) => Settings.timeout = Some(time.toInt)

        case regularityR("+") => Settings.regularity = true
        case regularityR("-") => Settings.regularity = false

        case instantiateR("+") => Settings.instantiate = true
        case instantiateR("-") => Settings.instantiate = false

        case essentialR("+") => Settings.essential = true
        case essentialR("-") => Settings.essential = false                    

        case pruneModelR("+") => Settings.prune_model = true
        case pruneModelR("-") => Settings.prune_model = false

        case progressPrintR("+") => Settings.progress_print = true
        case progressPrintR("-") => Settings.progress_print = false

        case fullTablePrintR("+") => Settings.full_table = true
        case fullTablePrintR("-") => Settings.full_table = false                    

        case hardCodedR(str) => {
          val steps = 
            for (tuple <- str.split(";")) yield {
              val split = tuple.split('.')
              (split(0).toInt, split(1).toInt)
            }
          Settings.hard_coded = Some(steps.toList)
        }

        case debugR("+") => Settings.debug = true
        case debugR("-") => Settings.debug = false

        case timeR("+") => Settings.time = true
        case timeR("-") => Settings.time = false

        case saveBreuR("+") => Settings.save_breu = true
        case saveBreuR("-") => Settings.save_breu = false                    

        case startMaxDepthR(maxDepth) => Settings.start_max_depth = maxDepth.toInt
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

