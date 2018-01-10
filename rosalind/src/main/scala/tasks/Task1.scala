package tasks

import tools.Solver

object Task1 extends Solver {
  import Solver._

  //Counts the number of occurences of a motif in a sequence
  def occCount(seq: String, motif: String): Int ={
    seq.sliding(motif.size).count(_ == motif)
  }

  override def solve(reader: Iterator[String]): Any = {
    val seq = reader.trimmedLine
    val motif = reader.trimmedLine
    occCount(seq,motif)
  }
}
