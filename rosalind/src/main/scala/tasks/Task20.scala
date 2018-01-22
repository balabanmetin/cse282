package tasks


import tasks.Task17._
import tasks.Task18._
import tools.Solver

import scala.util.Random


object Task20 extends Solver {

  import Solver._

  def profileRandomlyKmer(profile: Array[Array[Double]], text: String, k: Int): String = {
    val probDist = text.sliding(k).map(kmer => kmer.zipWithIndex.map(p => profile(nucToNum(p._1))(p._2)).product)
    if(probDist.sum == 0)
      text.sliding(k).next()
    else {
      val rand = Random.nextDouble * probDist.sum
      val cdf = probDist.scanLeft(0.0)(_ + _)
      val zppd = text.sliding(k).zip(cdf).toArray
      zppd.find(_._2 > rand).map(_._1).getOrElse(zppd.last._1)
    }
  }

  def gibbsSampler(dna: Array[String], k: Int, t: Int, N: Int): Array[String] = {
    val initMotif = dna.map(text => Random.shuffle(text.sliding(k).toList).head)
    val trials = for (j <- 0 until N) yield {
      val i = Random.nextInt(t)
      val profile = formProfileWithPrior(initMotif.take(i) ++ initMotif.drop(i + 1))
      initMotif.take(i) ++ Array(profileRandomlyKmer(profile, dna(i), k)) ++ initMotif.drop(i + 1)
    }
    (initMotif +: trials.toList).minBy(score)
  }

  override def solve(reader: Iterator[String]): Any = {
    val Array(k, t, n) = reader.nextIntArray
    val dna = reader.map(_.trim).toArray
    (for (i <- 0 until 20) yield gibbsSampler(dna, k, t, n)).minBy(score).mkString("\n")
  }
}
