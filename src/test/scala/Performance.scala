package bct;

// import org.scalatest.FunSuite
// import org.scalatest._
// import java.io.File
// import scala.io.Source



// class Performance extends FunSpec {

//   def getListOfFiles(dir: File, extensions: List[String]): List[File] = {
//     dir.listFiles.filter(_.isFile).toList.filter { file =>
//       extensions.exists(file.getName.endsWith(_))
//     }
//   }

//   val TIMEOUT = 10000

//   val sources = new File(getClass.getResource("/performance/").toURI())
//   val files = getListOfFiles(sources, List(".p"))

//   describe("Performance") {
//     for (f <- files) {      
//       it(f.getName()) {
//         val ret = Benchmarker.run(f.toString, TIMEOUT)
//         println(ret)
//         assert(ret == "TIMEOUT")
//       }
//     }    
//   }  
// }
