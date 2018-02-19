package tasks

import tools.Solver
import tasks.Task41.Alignment

import scala.io.Source

object Task42 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val seq1 = reader.trimmedLine
    val seq2 = reader.trimmedLine
    val alignment = localAlign(seq1, seq2, readScoringMap)
    alignment.score.toString ++ "\n" ++ alignment.seq1 ++ "\n" ++ alignment.seq2
  }

  def readScoringMap: Map[Char, Map[Char, Int]] = {
    val scoringIt: Iterator[String] = Source.fromResource("scoring.txt").getLines()
    val keys = scoringIt.nextStringArray.map(_.head)
    (for (i <- keys.indices) yield {
      val line = scoringIt.nextStringArray
      val key = line.head.head
      val value = keys.zip(line.tail.map(_.toInt)).toMap
      key -> value
    }).toMap
  }

  // Find the highest-scoring local alignment between two strings.
  def localAlign(seq1: String, seq2: String,
                      scoringMap: Map[Char, Map[Char, Int]],
                      indelPenalty: Int = -5): Alignment = {
    val dp: Array[Array[Int]] = Array.ofDim[Int](seq1.length + 1, seq2.length + 1)
    for(i <- seq1.indices)
      dp(i)(0) = 0
    for(i <- seq2.indices)
      dp(0)(i) = 0
    for(i <- 1 to seq1.length)
      for (j <- 1 to seq2.length)
        dp(i)(j) = List(0,
                        dp(i)(j - 1) + indelPenalty,
                        dp(i - 1)(j) + indelPenalty,
                        dp(i - 1)(j - 1) + scoringMap(seq1(i - 1))(seq2(j - 1))).max

    val opt = dp.map(_.max).max
    val sink = (for(i <- 0 to seq1.length ; j <- 0 to seq2.length) yield (i, j)).maxBy{case (i,j) => dp(i)(j)}

    def prepend(chars: (Char, Char), seqs: (List[Char], List[Char])): (List[Char], List[Char]) =
      (chars._1 :: seqs._1, chars._2 :: seqs._2)

    def backTrack(seq1: List[Char], seq2: List[Char], dp: Array[Array[Int]]): (List[Char], List[Char]) = {
      (seq1, seq2) match {
        case (a, b) =>
          if (dp(a.length)(b.length) == 0)
            (Nil, Nil)
          else if (dp(a.length)(b.length) == dp(a.length)(b.length - 1) + indelPenalty)
            prepend(('-', b.head), backTrack(a, b.tail, dp))
          else if (dp(a.length)(b.length) == dp(a.length - 1)(b.length) + indelPenalty)
            prepend((a.head, '-'), backTrack(a.tail, b, dp))
          else
            prepend((a.head, b.head), backTrack(a.tail, b.tail, dp))
      }
    }

    val alignment: (List[Char], List[Char]) = backTrack(seq1.toList.take(sink._1).reverse,
                                                        seq2.toList.take(sink._2).reverse, dp)
    Alignment(opt, alignment._1.reverse.mkString(""), alignment._2.reverse.mkString(""))
  }

}
