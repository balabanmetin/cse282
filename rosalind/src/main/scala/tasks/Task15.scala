package tasks

import tasks.Task9._
import tools.Solver

object Task14 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val seq = reader.trimmedLine
    val k = reader.nextInt
    generateDBall(seq,k).mkString("\n")
  }
}
