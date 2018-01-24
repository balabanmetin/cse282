package tasks

import tools.Solver
import tasks.Task26._

object Task27 extends Solver {

  override def solve(reader: Iterator[String]): Any = {
    val dna = reader.map(_.trim).toList
    printGraph(debruijnFromKmers(dna))
  }

  // Generate the k-mer composition of a string.
  def debruijnFromKmers(kmers: List[String]): Map[String, List[String]] = {
    val composition = kmers.map(kmer => (kmer.reverse.tail.reverse, kmer.tail))
    glue(composition)
  }
}
