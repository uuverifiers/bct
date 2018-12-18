package bct

object D {
  var debug = false
  var breuCount = 0
  def dprint(str : String) =
    if (debug) print(str)

  def dprintln(str : String) = dprint(str + "\n")  
}
