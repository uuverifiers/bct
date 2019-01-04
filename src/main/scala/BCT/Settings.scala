package bct

object Settings {
  var timeout = 60000 : Int
  var regularity = false
  var start_clause = None : Option[Int]
  var debug = false
  var progress_print = false


  def print() = {
    D.dlargeboxprintln("SETTINGS")
    println("Debug: " + debug)
    println("Progress Print: " + progress_print)    
    println("Timeout: " + timeout)
    println("Regularity: " + regularity)
    println("Start Clause: " + start_clause)
  }
}
