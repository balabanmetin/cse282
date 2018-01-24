package tasks

import tools.Solver

object Task5 extends Solver {

  import Solver._
  import Task4._

  override def solve(reader: Iterator[String]): Any = {
    val seq = reader.trimmedLine
    val params = reader.nextIntArray
    findClumps(seq, params(0), params(1), params(2)).mkString(" ")
  }

  //Find patterns forming clumps in a string.
  def findClumps(seq: String, k: Int, L: Int, t: Int): List[String] = {
    def checkClumps(seq: String, motif: String, L: Int, t: Int): Boolean = {
      val occuran = findOcc(seq, motif)
      occuran.sliding(t).exists(lst => (lst.size == t) && (lst.last - lst.head + 1) <= L)
    }

    seq.sliding(k).filter(motif => checkClumps(seq, motif, L, t)).toList.distinct
  }
}
