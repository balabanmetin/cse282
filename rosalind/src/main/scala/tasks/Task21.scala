package tasks

import tasks.Task7._
import tools.Solver

object Task21
  extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val kmer = reader.trimmedLine
    val dna = reader.nextStringArray.toList
    distanceBetweenPatternAndStrings(kmer, dna)
  }

  def distanceBetweenPatternAndStrings(kmer: String, dna: List[String]): Int = {
    dna.map(s => s.sliding(kmer.length).map(hamming(_, kmer)).min).sum
  }
}
