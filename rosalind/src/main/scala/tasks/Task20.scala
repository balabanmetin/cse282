package tasks


import tasks.Task17._
import tasks.Task18._
import tools.Solver

import scala.util.Random


object Task20 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val Array(k, t, n) = reader.nextIntArray
    val dna = reader.map(_.trim).toArray
    (for (i <- 0 until 20) yield gibbsSampler(dna, k, t, n)).minBy(score).mkString("\n")
  }

  def gibbsSampler(dna: Array[String], k: Int, t: Int, N: Int): Array[String] = {
    val initMotif = dna.map(text => Random.shuffle(text.sliding(k).toList).head)
    val trials = (0 until N).scanLeft(initMotif) { case (motif, j) =>
      val i = Random.nextInt(t)
      val profile = formProfileWithPrior(motif.take(i) ++ motif.drop(i + 1))
      motif.take(i) ++ Array(profileRandomlyKmer(profile, dna(i), k)) ++ motif.drop(i + 1)
    }
    trials.minBy(score)
  }

  def profileRandomlyKmer(profile: Array[Array[Double]], text: String, k: Int): String = {
    val probDist = text.sliding(k).toList.map(kmer => kmer.zipWithIndex.map(p => profile(nucToNum(p._1))(p._2)).product)
    if (probDist.sum == 0)
      text.sliding(k).next()
    else {
      val rand = Random.nextDouble * probDist.sum
      val cdf = probDist.scanLeft(0.0)(_ + _).tail
      val zppd = text.sliding(k).toList.zip(cdf).toArray
      zppd.find(_._2 > rand).map(_._1).getOrElse(zppd.last._1)
    }
  }
}
