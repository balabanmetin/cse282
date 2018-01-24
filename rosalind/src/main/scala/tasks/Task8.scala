package tasks

import tools.Solver

object Task8 extends Solver {

  import Solver._
  import Task7._

  override def solve(reader: Iterator[String]): Any = {
    val motif = reader.trimmedLine
    val seq = reader.trimmedLine
    val dist = reader.nextInt
    approxMatches(seq, motif, dist).mkString(" ")
  }

  //Find all approximate occurrences of a pattern in a string.
  def approxMatches(seq: String, motif: String, dist: Int): Iterator[Int] = {
    seq.sliding(motif.size).zipWithIndex.filter(pair => hamming(pair._1, motif) <= dist).map(_._2)
  }
}
