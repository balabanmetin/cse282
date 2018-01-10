package tasks

import tools.Solver

object Task3 extends Solver {
  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val seq = reader.trimmedLine
    val rule = Map('A'->'T','T'->'A','C'->'G','G'->'C')
    seq.map(a => rule(a))
  }
}
