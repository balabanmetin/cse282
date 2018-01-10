package tasks

import tools.Solver
import Task9._
import Task3._

object Task10 extends Solver {

  import Solver._

  def mostFrequentWithMismatchesAndReverse(seq: String, k: Int, d: Int): List[String] = {

    val groupsStraight = seq.sliding(k).flatMap(m => generateDBall(m, d)).toList.groupBy(identity)
    val groupsReverse = seq.sliding(k).flatMap(m => generateDBall(m, d)).toList.groupBy(reverseComplement)
    val straightCounts = groupsStraight.map(x => (x._1, x._2.length)).toSeq
    val reverseCounts = groupsReverse.map(x => (x._1, x._2.length)).toSeq
    val mergedCounts = (straightCounts ++ reverseCounts).groupBy(_._1).mapValues(_.map(_._2).sum)
    val maximum = mergedCounts.maxBy(_._2)
    mergedCounts.filter(_._2 == maximum._2).keys.toList
  }

  override def solve(reader: Iterator[String]): Any = {
    val seq = reader.trimmedLine
    val Array(k,dist) = reader.nextIntArray
    mostFrequentWithMismatchesAndReverse(seq, k, dist).mkString(" ")
  }
}
