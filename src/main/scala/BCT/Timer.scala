package bct

import scala.collection.mutable.{ArrayStack => Stack, HashMap, ArrayBuilder}
import scala.util.Sorting

/**
 * Object for measuring the time needed by the various tasks, methods, etc.
 * The object, in particular, supports nested operations that call each other
 * and correctly measures the time spent in each of the operations.
 */
object Timer {

  private var startTime : Long = _
  private val runningOps = new Stack[String]

  // acuumulated time spent in each operation
  private val accumulatedTimes = new HashMap[String, Long] {
    override def default(op : String) : Long = 0
  }
  // number of call to each operation
  private val callCounters = new HashMap[String, Int] {
    override def default(op : String) : Int = 0
  }
  
  private def addTime : Unit = {
    val now = System.nanoTime
    if (!runningOps.isEmpty) {
      val op = runningOps.top
      accumulatedTimes += (op -> (accumulatedTimes(op) + now - startTime))
    }
    startTime = now
  }

  def measure[A](op : String)(comp : => A) : A = {
    addTime
    callCounters += (op -> (callCounters(op) + 1))
    runningOps push op
    
    val res =
      try {
        comp
      } finally {
        addTime
        runningOps.pop
      }
    
    res
  }
  
  def reset : Unit = {
    accumulatedTimes.clear
    callCounters.clear
  }
  
  override def toString : String = {
    val resBuf = ArrayBuilder.make[(String, Int, Long)]

    for ((op, time) <- accumulatedTimes)
      resBuf += ((op, callCounters(op), time))

    val resAr = resBuf.result
    Sorting.stableSort(resAr)

    val table =
    (for ((op, count, time) <- resAr) yield {
      var paddedOp = op
      // HACK: for some reason, the <code>RichString.format</code> method does
      // not work
      while (paddedOp.size < 40)
        paddedOp = paddedOp + " "
      
      val timeInMS = time.toDouble / 1000000.0
          
      (paddedOp + "\t" + count + "\t" + timeInMS + "ms")
    }) mkString "\n"
    
    val totalTime = (0l /: accumulatedTimes.valuesIterator)(_ + _)
    val totalTimeInMS = totalTime.toDouble / 1000000.0
    
    val totalCalls = (0 /: callCounters.valuesIterator)(_ + _)
    
    val total = "Total: " + totalCalls + ", " + totalTimeInMS + "ms"
    
    table + "\n" + total
  }
  
}
