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
    val alignment = globalAlignment(seq1, seq2, scoringMap)
    alignment.score.toString ++ "\n" ++ alignment.seq1 ++ "\n" ++ alignment.seq2
  }


  case class Alignment(score: Int, seq1: String, seq2: String)

  // Find the highest-scoring alignment between two strings using a scoring matrix.
  def globalAlignment(seq1: String, seq2: String,
                      scoringMap: Map[Char, Map[Char, Int]],
                      indelPenalty: Int = -5): Alignment = {
    type Input = (List[Char], List[Char])
    type Cache = (Int, Int)
    type Output = Int
    type DP = Memo[Input, Cache, Output]
    implicit def ev(key: Input): Cache = (key._1.length, key._2.length)

    lazy val f: DP = Memo {
      case (a, Nil) => a.length * indelPenalty
      case (Nil, b) => b.length * indelPenalty
      case (a, b) => List(f(a, b.tail) + indelPenalty,
                          f(a.tail, b) + indelPenalty,
                          f(a.tail, b.tail) + scoringMap(a.head)(b.head)).max
    }
    val opt = f(seq1.toList, seq2.toList)

    def prepend(chars: (Char, Char), seqs: (List[Char], List[Char])): (List[Char], List[Char]) =
      (chars._1 :: seqs._1, chars._2 :: seqs._2)

    def backTrack(seq1: List[Char], seq2: List[Char], f: DP): (List[Char], List[Char]) = {
      (seq1, seq2) match {
        case (Nil, Nil) => (Nil, Nil)
        case (Nil, b :: bs) => prepend(('-', b), backTrack(Nil, bs, f))
        case (a :: as, Nil) => prepend((a, '-'), backTrack(as, Nil, f))
        case (a, b) =>
          if (f(a, b) == f(a, b.tail) + indelPenalty)
            prepend(('-', b.head), backTrack(a, b.tail, f))
          else if (f(a, b) == f(a.tail, b) + indelPenalty)
            prepend((a.head, '-'), backTrack(a.tail, b, f))
          else
            prepend((a.head, b.head), backTrack(a.tail, b.tail, f))
      }
    }

    val alignment: (List[Char], List[Char]) = backTrack(seq1.toList, seq2.toList, f)
    Alignment(opt, alignment._1.mkString(""), alignment._2.mkString(""))
  }
}
