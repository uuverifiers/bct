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
    "\tEssential backtracking: " + essential  + "\n" + 
    "\tStart Clause: " + start_clause + "\n" +
    "\tSolver Bits: " + solver_bits + "\n" +    
    "\tStart Max Depth: " + start_max_depth + "\n" +
    "\tRepeat: " + repeat + "\n" +
    "\tSaving BREU-problems: " + save_breu + "\n"+
    "\tAdd Unit Clauses: " + add_unit + "\n" +
    "\tAdd Constants: " + add_constants + "\n" +    
    "\tOnly Parse: " + only_parse
    "\tExtend Equalities: " + extend_equalities
  }
  var timeout = None : Option[Int]


  var regularity = false
  var instantiate = true
  var essential = false
  var add_unit = true
  var add_constants = true
  var extend_equalities = true

  var solver_bits = 8
  var start_max_depth = 2
  var repeat = 1


  var start_clause = None : Option[Int]


  var time = true
  var debug = false
  var only_parse = false
  var progress_print = false
  var full_table = false
  var save_breu = false

  var hard_coded = None : Option[List[(Int, Int)]]


  def print() = {
    D.dlargeboxprintln("SETTINGS")
    println(this)
  }
}
