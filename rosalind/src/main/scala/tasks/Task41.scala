package tasks

import tools.{Memo, Solver}

object Task41 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val seq1 = reader.trimmedLine
    val seq2 = reader.trimmedLine
    editDistance(seq1, seq2)
  }

  // Find the edit distance between two strings.
  def editDistance(seq1: String, seq2: String): Int = {
    type Input = (List[Char], List[Char])
    type Cache = (Int, Int)
    type Output = Int
    type DP = Memo[Input, Cache, Output]
    implicit def ev(key: Input): Cache = (key._1.length, key._2.length)

    lazy val f: DP = Memo {
      case (a, Nil) => a.length
      case (Nil, b) => b.length
      case (a :: as, b :: bs) if a == b => f(as, bs)
      case (a, b) => List(f(a, b.tail), f(a.tail, b), f(a.tail, b.tail)).min + 1
    }
    f(seq1.toList, seq2.toList)
  }
}
