package tasks

import tasks.Task16._
import tasks.Task17._
import tasks.Task18._
import tools.Solver

import scala.util.Random


object Task19 extends Solver {

  import Solver._

  def climb(motif: Array[String], dna: Array[String], k: Int): Array[String] = {
    val profile = formProfileWithPrior(motif)
    val motifnew = dna.map(profileMostProbable(_, k, profile))
    if (score(motifnew) < score(motif)) climb(motifnew, dna, k) else motif
  }

  def randomizedMotifSearch(dna: Array[String], k: Int, t: Int): Array[String] = {
    val initMotif = dna.map(text => Random.shuffle(text.sliding(k).toList).head)
    climb(initMotif, dna, k)
  }

  override def solve(reader: Iterator[String]): Any = {
    val Array(k, t) = reader.nextIntArray
    val dna = reader.map(_.trim).toArray
    (for (i <- 0 until 1000) yield randomizedMotifSearch(dna, k, t)).minBy(score).mkString("\n")
  }
}
