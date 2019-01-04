package bct

object Settings {
  var timeout = 60000 : Int


  var regularity = false
  var prune_model = true


  var start_clause = None : Option[Int]


  var debug = false
  var progress_print = false


  def print() = {
    D.dlargeboxprintln("SETTINGS")
    println("Debug: " + debug)
    println("Progress Print: " + progress_print)    
    println("Timeout: " + timeout)
    println("Regularity: " + regularity)
    println("Prune Model: " + prune_model)    
    println("Start Clause: " + start_clause)
  }
}
