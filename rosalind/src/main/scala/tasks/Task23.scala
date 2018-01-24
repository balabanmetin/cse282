package tasks

import tools.Solver

object Task23 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val k = reader.nextInt
    val seq = reader.trimmedLine
    kmerComposition(seq, k).mkString("\n")
  }

  // Generate the k-mer composition of a string.
  def kmerComposition(seq: String, k: Int): Iterator[String] = {
    seq.sliding(k)
  }
}
