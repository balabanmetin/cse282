package tasks

import tasks.Task7._
import tasks.Task9._
import tools.Solver

object Task14 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val Array(k, d) = reader.nextIntArray
    val dna = reader.map(_.trim).toList
    motifEnumeration(dna, k, d).mkString(" ")
  }

  //Implement MotifEnumeration (shown above) to find all (k, d)-motifs in a collection of strings.
  def motifEnumeration(dna: List[String], k: Int, d: Int): List[String] = {
    val candidates = dna.flatMap(text => text.sliding(k).flatMap(generateDBall(_, d))).distinct
    candidates.filter(c => dna.map(s => s.sliding(c.length).map(hamming(_, c)).min).max <= d)
  }
}
