package tasks

import tools.Solver
import tasks.Task50._

object Task51 extends Solver {

  override def solve(reader: Iterator[String]): Any = {
    val chr = readChromosome(reader)
    countBreakpoints(chr)
  }

  def countBreakpoints(chr: Array[Int]): Int = {
    val extchr = Array(0) ++ chr ++ Array(chr.length + 1)
    extchr.length - extchr.sliding(2).count(p => p(0) == p(1) - 1) - 1
  }
}
