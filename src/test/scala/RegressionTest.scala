package bct;

import org.scalatest.FunSuite
import org.scalatest._
import java.io.File
import scala.io.Source

class RegressionTest extends FunSpec {

  Settings.timeout = Some(30000)
  Settings.regularity = false
  Settings.instantiate = false
  Settings.essential = false
  Settings.start_max_depth = 5
  Settings.start_clause = None
  Settings.time = false
  Settings.debug = false
  Settings.progress_print = false
  Settings.full_table = false
  Settings.save_breu = false
  Settings.hard_coded = None

  val satSources = new File(getClass.getResource("/sat/").toURI()) + "/"

  describe("Simple") {
    val files = List(
      "SET002+3.p",
      "SET043+1.p",
      "SET055+1.p"
    ).map(satSources + _)

    for (f <- files) {
      it (f) {
        assert(Benchmarker.run(f) == "SAT")
      }
    }
  }

  describe("-instantiate +regularity") {
    val files = List(
      "SET002+3.p",
      "SET043+1.p",
      "SET044+1.p",
      "SET055+1.p"
    ).map(satSources + _)

    Settings.regularity = true

    for (f <- files) {
      it (f) {
        assert(Benchmarker.run(f) == "SAT")
      }
    }
  }  
}
