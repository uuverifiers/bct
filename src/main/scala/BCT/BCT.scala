package bct


object BCT extends App {
  def parseFile(problem : String) = {
    Parser.tptp2Internal(problem) match {
      case None => println("Error parsing..")
      case Some(pseudoClauses) => {
        println("Parsed")
        println("PseudoClauses:")
        for (pc <- pseudoClauses) {
          println(pc)
        }
      }
    }
  }

  if (args.length < 2) {
    println("Usage: [dir|file] path")
  } else {
    args(0) match {
      case "dir" => {
        val inputDir = args(1)
        val split = args(1).split('/').reverse
        val outFile = 
          (if (split(0) == "")
            split(1)
          else
            split(0)) + ".out"
        Benchmarker.testDir(inputDir, outFile)
      }
      case "file" => Benchmarker.testFile(args(1))
    }
  }
}
