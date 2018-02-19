package tasks

import tools.Solver
import tasks.Task50._

object Task51 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val chr = readChromosome(reader)
    countBreakpoints(chr)
  }

  def countBreakpoints(chr: Array[Int]): Int = {
    chr.length - chr.sliding(2).count(p => p(0) == p(1) - 1)
  }
}
