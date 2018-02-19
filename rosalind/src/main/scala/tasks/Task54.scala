package tasks

import tools.Solver
import tasks.Task3._


object Task54 extends Solver {

  import tools.Solver._

  override def solve(reader: Iterator[String]): Any = {
    val k = reader.nextInt
    val seq1 = reader.trimmedLine
    val seq2 = reader.trimmedLine
    sharedKmers(k, seq1, seq2).mkString("\n")
  }

  // Given two strings, find all their shared k-mers.
  def sharedKmers(k: Int, seq1: String, seq2: String): List[(Int,Int)] = {
    val kmers1 = seq1.sliding(k).toList.zipWithIndex
    val kmers2 = seq2.sliding(k).toList.zipWithIndex
    (for(x <- kmers1; y <- kmers2) yield (x, y)).collect{
      case ((k1, i),(k2, j)) if k1 == k2 || k1 == reverseComplement(k2) => (i, j)
    }
  }
}
