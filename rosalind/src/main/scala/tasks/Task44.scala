package tasks

import tools.Solver

import scala.io.Source

object Task44 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val seq1 = reader.trimmedLine
    val seq2 = reader.trimmedLine
    val alignment = overlapAlign(seq1, seq2, +1, -2, -2)
    alignment.score.toString ++ "\n" ++ alignment.seq1 ++ "\n" ++ alignment.seq2
  }


  case class Alignment(score: Int, seq1: String, seq2: String)


  // Construct a highest-scoring overlap alignment between two strings.


  def overlapAlign(seq1: String, seq2: String, matches: Int, mismatches: Int, indels: Int): Alignment = {
    val dp: Array[Array[Int]] = Array.ofDim[Int](seq1.length + 1, seq2.length + 1)
    for(i <- seq1.indices)
      dp(i)(0) = 0
    for(i <- seq2.indices)
      dp(0)(i) = i * indels
    for(i <- 1 to seq1.length)
      for (j <- 1 to seq2.length)
        dp(i)(j) = List(dp(i)(j - 1) + indels,
                        dp(i - 1)(j) + indels,
                        dp(i - 1)(j - 1) + (if(seq1(i - 1) == seq2(j - 1)) matches else mismatches)).max

    val opt = dp(seq1.length).max
    val sink = (0 to seq2.length).maxBy(dp(seq1.length)(_))


    def prepend(chars: (Char, Char), seqs: (List[Char], List[Char])): (List[Char], List[Char]) =
      (chars._1 :: seqs._1, chars._2 :: seqs._2)

    def backTrack(seq1: List[Char], seq2: List[Char], dp: Array[Array[Int]]): (List[Char], List[Char]) = {
      (seq1, seq2) match {
        case (_, Nil) => (Nil, Nil)
        case (Nil, b :: bs) => prepend(('-', b), backTrack(Nil, bs, dp))
        case (a, b) =>
          if (dp(a.length)(b.length) == dp(a.length)(b.length - 1) + indels)
            prepend(('-', b.head), backTrack(a, b.tail, dp))
          else if (dp(a.length)(b.length) == dp(a.length - 1)(b.length) + indels)
            prepend((a.head, '-'), backTrack(a.tail, b, dp))
          else
            prepend((a.head, b.head), backTrack(a.tail, b.tail, dp))
      }
    }

    val alignment: (List[Char], List[Char]) = backTrack(seq1.toList.reverse, seq2.toList.take(sink).reverse, dp)
    Alignment(opt, alignment._1.reverse.mkString(""), alignment._2.reverse.mkString(""))
  }

}
