package tasks

import tasks.Task16._
import tasks.Task17._
import tools.Solver

object Task18 extends Solver {

  import Solver._


  def formProfileWithPrior(motif: Array[String]): Array[Array[Double]] = {
    val trans = motif.map(_.toList).toList.transpose // ["ABC","DEF"] -> ["AD","BE","CF"]
    val nucProf = trans.map(_.groupBy(identity).mapValues(_.length))
    val transposedProf = nucProf.map(prof => List('A','C','G','T').map(c => (1.0 + prof.getOrElse(c,0)*1.0)/(motif.length + 4)))
    transposedProf.transpose.map(_.toArray).toArray
  }

  def greedyMotifSearchWithPrior(dna: Array[String], k: Int, t: Int): Array[String] = {
    val allMotifsFound = dna(0).sliding(k).map{case s =>
      dna.tail.foldLeft(Array(s)){ case (motif, text) =>
        motif :+ profileMostProbable(text, k, formProfileWithPrior(motif))
      }
    }
    allMotifsFound.minBy(score)
  }

  override def solve(reader: Iterator[String]): Any = {
    val Array(k, t) = reader.nextIntArray
    val dna = reader.map(_.trim).toArray
    greedyMotifSearchWithPrior(dna, k, t).mkString("\n")
  }
}
