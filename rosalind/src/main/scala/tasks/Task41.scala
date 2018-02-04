package tasks

import tools.{Memo, Solver}

import scala.io.Source

object Task41 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val seq1 = reader.trimmedLine
    val seq2 = reader.trimmedLine
    val scoringIt: Iterator[String] = Source.fromResource("scoring.txt").getLines()
    val keys = scoringIt.nextStringArray.map(_.head)
    val scoringMap = (for (i <- keys.indices) yield {
      val line = scoringIt.nextStringArray
      val key = line.head.head
      val value = keys.zip(line.tail.map(_.toInt)).toMap
      key -> value
    }).toMap
    val alignment = fastAlign(seq1, seq2, scoringMap)
    alignment.score.toString ++ "\n" ++ alignment.seq1 ++ "\n" ++ alignment.seq2
  }


  case class Alignment(score: Int, seq1: String, seq2: String)


  // Find the highest-scoring alignment between two strings using a scoring matrix.
  def fastAlign(seq1: String, seq2: String,
                      scoringMap: Map[Char, Map[Char, Int]],
                      indelPenalty: Int = -5): Alignment = {
    val dp: Array[Array[Int]] = Array.ofDim[Int](seq1.length + 1, seq2.length + 1)
    for(i <- seq1.indices)
      dp(i)(0) = i * indelPenalty
    for(i <- seq2.indices)
      dp(0)(i) = i * indelPenalty
    for(i <- 1 to seq1.length)
      for (j <- 1 to seq2.length)
        dp(i)(j) = List(dp(i)(j - 1) + indelPenalty,
                        dp(i - 1)(j) + indelPenalty,
                        dp(i - 1)(j - 1) + scoringMap(seq1(i - 1))(seq2(j - 1))).max

    val opt = dp(seq1.length)(seq2.length)

    def prepend(chars: (Char, Char), seqs: (List[Char], List[Char])): (List[Char], List[Char]) =
      (chars._1 :: seqs._1, chars._2 :: seqs._2)

    def backTrack(seq1: List[Char], seq2: List[Char], dp: Array[Array[Int]]): (List[Char], List[Char]) = {
      (seq1, seq2) match {
        case (Nil, Nil) => (Nil, Nil)
        case (Nil, b :: bs) => prepend(('-', b), backTrack(Nil, bs, dp))
        case (a :: as, Nil) => prepend((a, '-'), backTrack(as, Nil, dp))
        case (a, b) =>
          if (dp(a.length)(b.length) == dp(a.length)(b.length - 1) + indelPenalty)
            prepend(('-', b.head), backTrack(a, b.tail, dp))
          else if (dp(a.length)(b.length) == dp(a.length - 1)(b.length) + indelPenalty)
            prepend((a.head, '-'), backTrack(a.tail, b, dp))
          else
            prepend((a.head, b.head), backTrack(a.tail, b.tail, dp))
      }
    }

    val alignment: (List[Char], List[Char]) = backTrack(seq1.toList.reverse, seq2.toList.reverse, dp)
    Alignment(opt, alignment._1.reverse.mkString(""), alignment._2.reverse.mkString(""))
  }

}
