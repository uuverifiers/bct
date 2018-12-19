package bct;

import org.scalatest.FunSuite
import org.scalatest._
import java.io.File
import scala.io.Source

class Regression extends FunSpec {

  val TIMEOUT = 5000

  def getListOfFiles(dir: File, extensions: List[String]): List[File] = {
    dir.listFiles.filter(_.isFile).toList.filter { file =>
      extensions.exists(file.getName.endsWith(_))
    }
  }
  
  val satSources = new File(getClass.getResource("/sat/").toURI())
  val satFiles = getListOfFiles(satSources, List(".p"))

  describe("SAT") {
    for (f <- satFiles) {
      it(f.getName()) {
        val ret = Benchmarker.run(f.toString, TIMEOUT)
        assert(ret == "SAT")
      }
    }
  }

  // val unknownSources = new File(getClass.getResource("/unknown/").toURI())
  // val unknownFiles = getListOfFiles(unknownSources, List(".p"))

  // describe("UNKNOWN") {
  //   for (f <- unknownFiles) {      
  //     it(f.getName()) {
  //       val ret = Benchmarker.run(f.toString, TIMEOUT)
  //       assert(ret == "UNKNOWN")
  //     }
  //   }    
  // }

  val timeoutSources = new File(getClass.getResource("/timeout/").toURI())
  val timeoutFiles = getListOfFiles(timeoutSources, List(".p"))

  describe("TIMEOUT") {
    for (f <- timeoutFiles) {      
      it(f.getName()) {
        val ret = Benchmarker.run(f.toString, TIMEOUT)
        assert(ret == "TIMEOUT")
      }
    }    
  }  
}
