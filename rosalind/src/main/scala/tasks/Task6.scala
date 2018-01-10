package tasks

import tools.Solver

object Task6 extends Solver {
  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val seq = reader.trimmedLine
    val indexed = seq.foldLeft(List(0))((acc,c) => {
      if(c == 'C') {
        acc :+ (acc.last - 1)
      }
      else if(c == 'G'){
        acc :+ (acc.last + 1)
      }
      else {
        acc :+ acc.last
      }
    }).zipWithIndex
    indexed.filter(v => v._1 == indexed.min._1).map(_._2).mkString(" ")
  }
}
