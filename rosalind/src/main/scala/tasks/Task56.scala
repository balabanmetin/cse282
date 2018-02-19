package tasks

import tasks.Task50._
import tools.Solver

object Task56 extends Solver {

  override def solve(reader: Iterator[String]): Any = {
    val chr = readChromosome(reader)
    chromosomeToString(cycleToChromosome(chr))
  }

  def cycleToChromosome(chr: Array[Int]): Array[Int] = {
    if(chr.isEmpty) Array.empty[Int]
    else {
      val Array(a,b) = chr.take(2)
      (if(a < b) Array(b/2) else Array(-a/2)) ++ cycleToChromosome(chr.drop(2))
    }
  }
}
