package tasks

import tools.{Memo, Solver}

object Task39 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val seq1 = reader.trimmedLine
    val seq2 = reader.trimmedLine
    longestCommonSubsequence(seq1, seq2)
  }

  // A longest common subsequence of these strings.
  def longestCommonSubsequence(seq1: String, seq2: String): String = {
    type Input = (List[Char], List[Char])
    type Cache = (Int, Int)
    type Output = List[Char]
    type DP = Memo[Input, Cache, Output]
    implicit def ev(key: Input): Cache = (key._1.length, key._2.length)

    lazy val f: DP = Memo {
      case (Nil, _) | (_, Nil) => Nil
      case (a :: as, b :: bs) if a == b => a :: f(as, bs)
      case (a, b) => List(f(a, b.tail), f(a.tail, b)).maxBy(_.length)
    }
    f(seq1.toList, seq2.toList).mkString("")
  }
}
