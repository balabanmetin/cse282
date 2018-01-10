package tasks

import tools.Solver

object Task7 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val seq1 = reader.trimmedLine
    val seq2 = reader.trimmedLine
    seq1.zip(seq2).count(tpl => tpl._1 != tpl._2)
  }
}
