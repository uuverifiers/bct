package bct;

import org.scalatest.FunSuite
import org.scalatest._
import java.io.File
import scala.io.Source

class RegressionTest extends FunSpec {

  Settings.timeout = Some(30000)

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
        val ret = Benchmarker.run(f.toString)
        assert(ret == "SAT")
      }
    }
  }
}
