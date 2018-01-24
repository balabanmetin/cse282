package tasks

import tools.Solver

object Task7 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val seq1 = reader.trimmedLine
    val seq2 = reader.trimmedLine
    hamming(seq1, seq2)
  }

  //Compute the Hamming distance between two DNA strings.
  def hamming(seq1: String, seq2: String): Int = {
    seq1.zip(seq2).count(tpl => tpl._1 != tpl._2)
  }
}
