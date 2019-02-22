package bct

object Settings {
  var timeout = None : Option[Int]


  var regularity = false
  var prune_model = true
  var instantiate = true
  var essential = false
  

  var start_max_depth = 5


  var start_clause = None : Option[Int]

  var breu = "new"

  var time = true
  var debug = false
  var progress_print = false
  var full_table = false
  var save_breu = false

  var hard_coded = None : Option[List[(Int, Int)]]


  def print() = {
    D.dlargeboxprintln("SETTINGS")
    println("Debug: " + debug)
    println("Time: " + time)    
    println("Hard-coded: " + hard_coded)
    println("Progress Print: " + progress_print)
    println("Print Full Table: " + full_table)        
    println("Timeout: " + timeout)
    println("Regularity: " + regularity)
    println("Prune Model: " + prune_model)
    println("Instantiate: " + instantiate)
    println("Eessential backtracking: " + essential) 
    println("Start Clause: " + start_clause)
    println("Start Max Depth: " + start_max_depth)
    println("BREU: " + breu)    
    println("Saving BREU-problems: " + save_breu)
  }
}
