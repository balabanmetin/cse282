package tasks

import tasks.Task16._
import tasks.Task7._
import tools.Solver

object Task17 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val Array(k, t) = reader.nextIntArray
    val dna = reader.map(_.trim).toArray
    greedyMotifSearch(dna, k, t).mkString("\n")
  }

  def greedyMotifSearch(dna: Array[String], k: Int, t: Int): Array[String] = {
    val allMotifsFound = dna(0).sliding(k).map { case s =>
      dna.tail.foldLeft(Array(s)) { case (motif, text) =>
        motif :+ profileMostProbable(text, k, formProfile(motif))
      }
    }
    allMotifsFound.minBy(score)
  }

  def score(motif: Array[String]): Int = {
    val cs = consensus(motif)
    motif.map(hamming(_, cs)).sum
  }

  def consensus(motif: Array[String]): String = {
    val trans = motif.map(_.toList).toList.transpose // ["ABC","DEF"] -> ["AD","BE","CF"]
    trans.map(_.groupBy(identity).maxBy(_._2.size)._1).mkString("")
  }

  def formProfile(motif: Array[String]): Array[Array[Double]] = {
    val trans = motif.map(_.toList).toList.transpose
    // ["ABC","DEF"] -> ["AD","BE","CF"]
    val nucProf = trans.map(_.groupBy(identity).mapValues(_.length))
    val transposedProf = nucProf.map(prof => List('A', 'C', 'G', 'T').map(c => prof.getOrElse(c, 0) * 1.0 / motif.length))
    transposedProf.transpose.map(_.toArray).toArray
  }
}
