package tasks

import tasks.Task50._
import tools.Solver

object Task55 extends Solver {

  override def solve(reader: Iterator[String]): Any = {
    val chr = readChromosome(reader)
    "(" ++ chromosomeToCycle(chr).mkString(" ") ++ ")"
  }

  def chromosomeToCycle(chr: Array[Int]): Array[Int] = {
    def extremities(i: Int): Array[Int] = if(i > 0) Array(2*i-1, 2*i) else Array(-2*i, -2*i-1)
    chr.flatMap(extremities)
  }
}
