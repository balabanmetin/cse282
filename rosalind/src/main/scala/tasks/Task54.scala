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

  def hashMapKmers(k: Int, seq: String): Map[String, List[Int]] = {
    val kmers1 = seq.sliding(k).zipWithIndex
    kmers1.toList.groupBy{case (h, i) => h}.mapValues(_.map(_._2))
  }

  // Given two strings, find all their shared k-mers.
  def sharedKmers(k: Int, seq1: String, seq2: String): List[(Int,Int)] = {
    val map2 = hashMapKmers(k, seq2)
    val map1 = hashMapKmers(k, seq1)
    map2.keys.collect{
      case key if map1.contains(key) => for(i <- map1(key); j <- map2(key)) yield (i, j)
      case key if map1.contains(reverseComplement(key)) => for(i <- map1(reverseComplement(key)); j <- map2(key)) yield (i, j)
    }.flatten.toList
  }
}
