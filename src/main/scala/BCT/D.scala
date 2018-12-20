package bct

import Console.{GREEN, RED, RESET, YELLOW, UNDERLINED}

object D {
  val MARGIN = 2
  var debug = false
  var breuCount = 0
  def dprint(str : String) =
    if (debug) print(str)

  val GREEN = Console.GREEN
  val YELLOW = Console.YELLOW

  def dprintln(str : String) = cPrintln(str)

  def colorString(str : String, color : String) = {
    color match {
      case "YELLOW" => s"${RESET}${YELLOW}" + str + s"${RESET}"
      case "" => str
    }
  }

  def dboxprintln(str : String, color : String = "") = {
    val cstr = colorString(str, color)

    dprintln("+" + ("-"*(MARGIN + str.length + MARGIN)) + "+")
    dprintln("|" + (" "*MARGIN) + cstr + (" "*MARGIN) + "|")
    dprintln("+" + ("-"*(MARGIN + str.length + MARGIN)) + "+")    
  }

  def dlargeboxprintln(str : String) = {
    cPrintln("+" + ("-"*(2*MARGIN + str.length + 2*MARGIN)) + "+")
    dprintln("|" + (" "*2*MARGIN) + (" "*str.length) + (" "*2*MARGIN) + "|")    
    dprintln("|" + (" "*2*MARGIN) + str + (" "*2*MARGIN) + "|")
    dprintln("|" + (" "*2*MARGIN) + (" "*str.length) + (" "*2*MARGIN) + "|")
    dprintln("+" + ("-"*(2*MARGIN + str.length + 2*MARGIN)) + "+")    
  }

  def cPrintln(str : String) = {
    if (debug)
      Console.println(str)    
  }

  def dprintclause(pc : PseudoClause, idx : Int) = {
    val clauseStrings =
      for ((pl, i) <- pc.zipWithIndex) yield {
        if (i == idx)
          s"${RESET}${GREEN}" + pl + s"${RESET}"
        else
          pl.toString
      }
    val str = pc.funEquations.mkString("^") + " :: " + clauseStrings.mkString(" v ")

    dprintln("+" + ("-"*(MARGIN + (str.length-13) + MARGIN)) + "+")
    dprintln("|" + (" "*MARGIN) + str + (" "*MARGIN) + "|")
    dprintln("+" + ("-"*(MARGIN + (str.length-13) + MARGIN)) + "+")
    
  }
}
