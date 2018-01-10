package tasks

import tools.Solver

object Task4 extends Solver {
  import Solver._

  //Find all occurrences of a pattern in a string.
  def findOcc(seq: String, motif:String): Iterator[Int] = {
    seq.sliding(motif.size).zipWithIndex.filter(_._1 == motif).map(_._2)
  }

  override def solve(reader: Iterator[String]): Any = {
    val motif = reader.trimmedLine
    val seq = reader.trimmedLine
    findOcc(seq,motif).mkString(" ")
  }
}
