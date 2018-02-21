package tasks

import tasks.Task59._
import tasks.Task50._
import tools.Solver

object Task58 extends Solver {

  override def solve(reader: Iterator[String]): Any = {
    val graph = readGraph(reader)
    graphToGenome(graph).map(chromosomeToString).mkString("")
  }

  // Solve the Graph To Genome Problem.
  def graphToGenome(graph: Seq[(Int, Int)]): List[Array[Int]] = {
    val head = graph.head
    _graphToGenome(graph.toSet - head, head, Nil)
  }

  def _graphToGenome(graph: Set[(Int, Int)], currentEdge: (Int, Int), currentChr: List[Int]): List[Array[Int]] = {
    val next = graph.filter{case (i,j) => i == currentEdge._2 + (if (currentEdge._2 % 2 == 0) -1 else 1)} // i.e. 4 => 3, 3 => 4
    val sign = if (currentEdge._1 % 2 == 0) 1 else -1
    val newChr = sign * (currentEdge._1 + 1) / 2 :: currentChr
    if (next.nonEmpty) {
      _graphToGenome(graph - next.head, next.head, newChr)
    } else {
      if (graph.isEmpty) {
        List(newChr.reverse.toArray)
      } else {
        val head = graph.head
        newChr.reverse.toArray :: _graphToGenome(graph - head, head, Nil)
      }
    }
  }
}
