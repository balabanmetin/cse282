package tasks

import tools.Solver

object Task8 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val motif = reader.trimmedLine
    val seq = reader.trimmedLine
    val dist = reader.nextInt

    def hamming(seq1: String, seq2: String): Int = {
      seq1.zip (seq2).count (tpl => tpl._1 != tpl._2)
    }
    seq.sliding(motif.size).zipWithIndex.filter(pair => hamming(pair._1,motif)<=dist).map(_._2).mkString(" ")
  }
}
