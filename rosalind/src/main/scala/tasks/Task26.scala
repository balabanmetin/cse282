package tasks

import tools.Solver

object Task26 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val k = reader.nextInt
    val seq = reader.trimmedLine
    printGraph(debruijn(seq, k-1))
  }

  // Generate the k-mer composition of a string.
  def debruijn(seq: String, k: Int): Map[String, List[String]] = {
    val composition = seq.sliding(k).zip(seq.sliding(k).drop(1)).toList
    composition.groupBy(_._1).mapValues(_.map(_._2))
  }

  def printGraph(graph: Map[String, List[String]]): String = {
    graph.mapValues(_.mkString(",")).map{ case (x,y) => x ++ " -> " ++ y}.mkString(" \n")
  }
}
