package tasks

import tools.Solver

object Task4 extends Solver {
  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val motif = reader.trimmedLine
    val seq = reader.trimmedLine
    seq.sliding(motif.size).zipWithIndex.filter(_._1 == motif).map(_._2).mkString(" ")
  }
}
