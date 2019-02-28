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
    val addUnitR = "(\\+|-)add-unit".r    
    val instantiateR = "(\\+|-)instantiate".r
    val essentialR = "(\\+|-)essential".r    
    val startMaxDepthR = "-start-max-depth=(\\d+)".r
    val repeatR = "-repeat=(\\d+)".r    
    val progressPrintR = "(\\+|-)progress".r
    val fullTablePrintR = "(\\+|-)full-table".r        
    val startClauseR = "-start-clause=(\\d+)".r
    val bitsR = "-bits=(\\d+)".r    
    val hardCodedR = "-hard-coded=([-]?\\d+\\.\\d+[;[-]?\\d+\\.\\d+]*)".r
    val debugR = "(\\+|-)debug".r
    val onlyParseR = "(\\+|-)only-parse".r
    val timeR = "(\\+|-)time".r    
    val saveBreuR = "(\\+|-)save-breu".r    

    if (!args.isEmpty) {
      args.head match {
        case timeoutR(_, time) => Settings.timeout = Some(time.toInt)

        case regularityR("+") => Settings.regularity = true
        case regularityR("-") => Settings.regularity = false

        case addUnitR("+") => Settings.add_unit = true
        case addUnitR("-") => Settings.add_unit = false          

        case instantiateR("+") => Settings.instantiate = true
        case instantiateR("-") => Settings.instantiate = false

        case essentialR("+") => Settings.essential = true
        case essentialR("-") => Settings.essential = false                    

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

        case onlyParseR("+") => Settings.only_parse = true
        case onlyParseR("-") => Settings.only_parse = false

        case timeR("+") => Settings.time = true
        case timeR("-") => Settings.time = false

        case saveBreuR("+") => Settings.save_breu = true
        case saveBreuR("-") => Settings.save_breu = false                    

        case startMaxDepthR(maxDepth) => Settings.start_max_depth = maxDepth.toInt
        case bitsR(b) => Settings.solver_bits = b.toInt          
        case repeatR(r) => Settings.repeat = r.toInt
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
    if (Settings.only_parse) {
      val Some((pseudoClauses, globalConstants)) = Parser.tptp2Internal(i)
      val strs = List(
        "+-------------------+",
        "|  PseudoClauses:   |",
        "+-------------------+") ++
      (for ((pc, i) <- pseudoClauses.zipWithIndex) yield {
        "(%3d)\t%s".format(i, pc)
      })

      D.dprintln(strs.mkString("\n"))
      D.dprintln("\n\n")
    } else {
      println(Benchmarker.benchmark(i, Settings.repeat))
    }
  }
}

