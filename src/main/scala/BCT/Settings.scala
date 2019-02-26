package bct

object Settings {


  override def toString = {
    "\tDebug: " + debug + "\n" + 
    "\tTime: " + time     + "\n" + 
    "\tHard-coded: " + hard_coded + "\n" + 
    "\tProgress Print: " + progress_print + "\n" + 
    "\tPrint Full Table: " + full_table         + "\n" + 
    "\tTimeout: " + timeout + "\n" + 
    "\tRegularity: " + regularity + "\n" + 
    "\tInstantiate: " + instantiate + "\n" + 
    "\tEessential backtracking: " + essential  + "\n" + 
    "\tStart Clause: " + start_clause + "\n" + 
    "\tStart Max Depth: " + start_max_depth + "\n" + 
    "\tSaving BREU-problems: " + save_breu
  }
  var timeout = None : Option[Int]


  var regularity = false
  var instantiate = true
  var essential = false
  

  var start_max_depth = 2


  var start_clause = None : Option[Int]

  var time = true
  var debug = false
  var progress_print = false
  var full_table = false
  var save_breu = false

  var hard_coded = None : Option[List[(Int, Int)]]


  def print() = {
    D.dlargeboxprintln("SETTINGS")
    println(this)
  }
}
