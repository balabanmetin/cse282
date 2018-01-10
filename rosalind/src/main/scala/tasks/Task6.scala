package tasks

import tools.Solver

object Task6 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val seq = reader.trimmedLine
    val indexed = seq.foldLeft((List(0), 0))((acc, c) =>
      c match {
        case 'C' => (acc._1 :+ (acc._2 - 1), acc._2 - 1)
        case 'G' => (acc._1 :+ (acc._2 + 1), acc._2 + 1)
        case _ => (acc._1 :+ acc._2, acc._2)
      })._1.zipWithIndex
    println("Checkpoint")
    indexed.filter(v => v._1 == indexed.min._1).map(_._2).mkString(" ")
  }
}
