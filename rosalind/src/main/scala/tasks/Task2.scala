package tasks

import tools.Solver

object Task2 extends Solver {

  import Solver._

  //Find the most frequent k-mers in a string.
  def mostFrequentWords(seq: String, k: Int): List[String] = {
    val counts = seq.sliding(k).map(kmer => kmer -> seq.sliding(k).count(_ == kmer)).toList
    val res = counts.filter(_._2 == counts.maxBy(_._2)._2).distinct
    res.map(_._1)
  }

  override def solve(reader: Iterator[String]): Any = {
    val seq = reader.trimmedLine
    val k = reader.nextInt
    mostFrequentWords(seq, k).mkString(" ")
  }
}
