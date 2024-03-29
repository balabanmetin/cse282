package tasks

import tasks.Task11._
import tasks.Task21._
import tools.Solver

object Task15
  extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val k = reader.nextInt
    val dna = reader.map(_.trim).toList
    medianString(dna, k)
  }

  def medianString(dna: List[String], k: Int): String = {
    val kmers = allkmers(k, List("A", "C", "G", "T"))
    val scores = kmers.map(distanceBetweenPatternAndStrings(_, dna))
    kmers.zip(scores).minBy(_._2)._1
  }
}
