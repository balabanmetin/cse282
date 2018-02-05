package tasks

import tools.Solver
import tasks.Task41.Alignment


object Task43 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val seq1 = reader.trimmedLine
    val seq2 = reader.trimmedLine
    val alignment = fittingAlign(seq1, seq2, +1, -1, -1)
    alignment.score.toString ++ "\n" ++ alignment.seq1 ++ "\n" ++ alignment.seq2
  }

  // Construct a highest-scoring fitting alignment between two strings.
  def fittingAlign(seq1: String, seq2: String, matches: Int, mismatches: Int, indels: Int): Alignment = {
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

    val opt = dp.map(r => r(seq2.length)).max
    val sink = (0 to seq1.length).maxBy(dp(_)(seq2.length))

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

    val alignment: (List[Char], List[Char]) = backTrack(seq1.toList.take(sink).reverse, seq2.toList.reverse, dp)
    Alignment(opt, alignment._1.reverse.mkString(""), alignment._2.reverse.mkString(""))
  }

}
