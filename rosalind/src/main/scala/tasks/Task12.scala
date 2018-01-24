package tasks

import tools.Solver

object Task12 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val seq = reader.trimmedLine
    hash(seq)
  }

  //Convert a DNA string to a number.
  def hash(seq: String): Long = {
    seq.reverse.foldLeft((0L, 0)) { case ((acc, expon), c) =>
      (acc + Math.pow(4, expon).toLong * nucToNum(c), expon + 1)
    }._1
  }
}
