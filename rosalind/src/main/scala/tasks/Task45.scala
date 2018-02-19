package tasks

import tools.{Memo, Solver}

object Task45 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val seq1 = reader.trimmedLine
    val seq2 = reader.trimmedLine
    val seq3 = reader.trimmedLine
    longestCommonSubsequence3(seq1, seq2, seq3).productIterator.toList.mkString("\n")
  }

  case class Alignment3(score: Int, seq1: String, seq2: String, seq3: String)

  // A longest common subsequence of three strings.
  def longestCommonSubsequence3(seq1: String, seq2: String, seq3: String): Alignment3 = {
    type Input = (List[Char], List[Char], List[Char])
    type Cache = (Int, Int, Int)
    type Output = (List[Char], List[Char], List[Char])
    type DP = Memo[Input, Cache, Output]
    implicit def ev(key: Input): Cache = (key._1.length, key._2.length, key._3.length)

    lazy val f: DP = Memo {
      case (a, Nil, Nil) => (a, List.fill(a.length)('-'), List.fill(a.length)('-'))
      case (Nil, b, Nil) => (List.fill(b.length)('-'), b, List.fill(b.length)('-'))
      case (Nil, Nil, c) => (List.fill(c.length)('-'), List.fill(c.length)('-'), c)
      case (Nil, b :: bs, c :: cs) => prepend(('-', b, c), f(Nil, bs, cs))
      case (a :: as, Nil, c :: cs) => prepend((a, '-', c), f(as, Nil, cs))
      case (a :: as, b :: bs, Nil) => prepend((a, b, '-'), f(as, bs, Nil))
      case (a :: as, b :: bs, c :: cs) if a == b && a == c => prepend((a, b, c), f(as, bs, cs))
      case (a, b, c) => List(prepend((a.head, '-', '-'),f(a.tail, b, c)),
                             prepend(('-', b.head, '-'),f(a, b.tail, c)),
                             prepend(('-', '-', c.head),f(a, b, c.tail))).maxBy(score)
    }
    val aln = f(seq1.toList, seq2.toList, seq3.toList)
    Alignment3(score(aln), aln._1.mkString(""), aln._2.mkString(""), aln._3.mkString(""))
  }

  def prepend(chars: (Char, Char, Char), seqs: (List[Char], List[Char], List[Char])):
                                                        (List[Char], List[Char], List[Char]) =
    (chars._1 :: seqs._1, chars._2 :: seqs._2, chars._3 :: seqs._3)

  def score(tuple: (List[Char], List[Char], List[Char])): Int = {
    List(tuple._1, tuple._2, tuple._3).transpose.count(chars => chars.forall(_ == chars.head))
  }

}


