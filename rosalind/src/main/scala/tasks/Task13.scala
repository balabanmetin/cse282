package tasks

import tools.Solver
import Task9._

object Task13 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val seq = reader.trimmedLine
    val k = reader.nextInt
    generateDBall(seq, k).mkString("\n")
  }
}
