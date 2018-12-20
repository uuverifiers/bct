package bct


object BCT extends App {
  if (args.length < 2) {
    println("Usage: [dir|file|parse] path [timeout] [startClause]")
  } else {
    val TIMEOUT =
      if (args.length < 3)
        5000
      else
        args(2).toLong

    val START_CLAUSE =
      if (args.length < 4)
        None
      else
        Some(args(3).toInt)
    args(0) match {
      case "dir" => {
        val inputDir = args(1)
        val split = args(1).split('/').reverse
        val outFile = 
          (if (split(0) == "")
            split(1)
          else
            split(0)) + ".out"
        Benchmarker.testDir(inputDir, outFile, TIMEOUT)
      }
      case "file" => {
        Benchmarker.testFile(args(1), TIMEOUT, START_CLAUSE)
      }

      case "parse" => {
        Benchmarker.parseFile(args(1))
      }
    }
  }
}

