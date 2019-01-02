package bct

import scala.collection.mutable.ListBuffer


object BCT extends App {


  val inputBuffer = ListBuffer() : ListBuffer[String]

  //
  // OPTIONS
  //
  var timeout = 5000
  var regularity = false
  var start_clause = None : Option[Int]

  def handleArguments(args : List[String]) : Unit = {
    val timeoutR = "-(timeout|to)=(\\d+)".r
    val regularityR = "(\\+|-)regularity".r
    val startClauseR = "-start-clause=(\\d+)".r

    if (!args.isEmpty) {
      args.head match {
        case timeoutR(_, time) => timeout = time.toInt

        case regularityR("+") => regularity = true
        case regularityR("-") => regularity = false

        case startClauseR(idx) => start_clause = Some(idx.toInt)

        case str => inputBuffer += str
      }
      handleArguments(args.tail)
    }
  }

  handleArguments(args.toList)

  val inputs = inputBuffer.toList.reverse  


  println("TIMEOUT: " + timeout)
  println("REGULARITY: " + regularity)
  println("INPUTS:")
  for (i <- inputs)
    println("\t" + i)

  for (i <- inputs) {
    Benchmarker.testFile(i, timeout, start_clause)
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

