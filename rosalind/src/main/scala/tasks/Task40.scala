package tasks

import tools.{Memo, Solver}

object Task40 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val seq1 = reader.trimmedLine
    val seq2 = reader.trimmedLine
    editDistance(seq1, seq2)
  }

  // Find the edit distance between two strings.
  def editDistance(seq1: String, seq2: String): Int = {
    val dp: Array[Array[Int]] = Array.ofDim[Int](seq1.length + 1, seq2.length + 1)
    for(i <- seq1.indices)
      dp(i)(0) = i
    for(i <- seq2.indices)
      dp(0)(i) = i
    for(i <- 1 to seq1.length)
      for (j <- 1 to seq2.length)
        dp(i)(j) = List(dp(i)(j - 1) + 1,
          dp(i - 1)(j) + 1,
          dp(i - 1)(j - 1) + (if (seq1(i - 1) == seq2(j - 1)) 0 else 1)).min
    dp(seq1.length)(seq2.length)
  }
}
