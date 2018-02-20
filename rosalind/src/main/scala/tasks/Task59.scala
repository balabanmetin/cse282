package tasks

import tasks.Task55._
import tasks.Task57._
import scala.util.matching._
import tools.Solver

object Task59 extends Solver {

  import tools.Solver._

  def readGraph(reader: Iterator[String]): List[(Int, Int)] = {
    val edge = raw"\(\d+,\s\d+\)".r
    val graphString = for (m <- edge.findAllMatchIn(reader.trimmedLine)) yield m.group(0)
    graphString.map(_.drop(1).dropRight(1).split(", ")).map(arr => (arr(0).toInt, arr(1).toInt)).toList
  }

  override def solve(reader: Iterator[String]): Any = {
    val graph = readGraph(reader)
    val Array(i1, i2, j1, j2) = reader.trimmedLine.split(", ").map(_.toInt)
    singleDCJop(graph, i1, i2, j1, j2).map(p => s"(${p._1}, ${p._2})").mkString(", ")
  }

  // Solve the 2-Break On Genome Graph Problem.
  def singleDCJop(graph: List[(Int,Int)], i1: Int, i2: Int, j1: Int, j2: Int): List[(Int, Int)] = {
    graph.map{
      case (a, b) if a == j2 => (i2, b)
      case (a, b) if a == i2 => (j2, b)
      case (a, b) if b == j2 => (a, i2)
      case (a, b) if b == i2 => (a, j2)
      case (a, b) => (a, b)
    }
  }
}
