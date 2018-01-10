package tasks

import tools.Solver

object Task9 extends Solver {

  import Solver._

  def mostFrequentWithMismatches(seq: String, k: Int, d: Int): List[String] = {
    def generate1Ball(seq: String): List[String] = {
      (0 until seq.length).flatMap(i =>
        List('A', 'T', 'G', 'C').map(c => seq.substring(0, i) + c + seq.substring(i + 1))
      ).toList.distinct
    }

    def generateDBall(seq: String, d: Int): List[String] = {
      if (d == 0)
        List(seq)
      else {
        val smallerBalls = generateDBall(seq, d - 1)
        smallerBalls.flatMap(s => generate1Ball(s)).distinct
      }
    }

    val groups = seq.sliding(k).flatMap(m => generateDBall(m, d)).toList.groupBy(identity)
    val counts = groups.map(x => (x._1, x._2.length))
    val maximum = counts.maxBy(_._2)
    counts.filter(_._2 == maximum._2).keys.toList
  }

  override def solve(reader: Iterator[String]): Any = {
    val seq = reader.trimmedLine
    val Array(k,dist) = reader.nextIntArray
    mostFrequentWithMismatches(seq, k, dist).mkString(" ")
  }
}
