package tasks

import tools.Solver

object Task1 extends Solver {
  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val seq = reader.trimmedLine
    val motif = reader.trimmedLine
    seq.sliding(motif.size).count(_ == motif)
  }
}
