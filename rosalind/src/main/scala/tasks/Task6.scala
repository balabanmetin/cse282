package tasks

import tools.Solver

object Task6 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val seq = reader.trimmedLine
    val indexed = seq.foldLeft(List(0))((acc, c) =>
      c match {
        case 'C' => (acc.head - 1)  :: acc
        case 'G' => (acc.head + 1) :: acc
        case _ => acc.head :: acc
      }).reverse.zipWithIndex
    val m = indexed.min._1
    indexed.filter(v => v._1 == m).map(_._2).mkString(" ")
  }
}
