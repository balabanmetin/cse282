package tasks

import tools.Solver

object Task3 extends Solver {
  import Solver._

  //Find the reverse complement of a DNA string.
  def reverseComplement(seq: String): String = {
    val rule = Map('A'->'T','T'->'A','C'->'G','G'->'C')
    seq.map(a => rule(a)).reverse.toString
  }

  override def solve(reader: Iterator[String]): Any = {
    val seq = reader.trimmedLine
    reverseComplement(seq)
  }
}
