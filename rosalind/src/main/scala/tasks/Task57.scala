package tasks

import tasks.Task55._
import tools.Solver

object Task57 extends Solver {

  import tools.Solver._

  override def solve(reader: Iterator[String]): Any = {
    val dna = reader.trimmedLine.split("(?<=\\))").map(_.drop(1).dropRight(1).split("\\s+").map(_.toInt))
    coloredEdges(dna).map(p => s"(${p._1}, ${p._2})").mkString(", ")
  }

  def makeCircular(chr: Array[Int]): Array[Int] = chr ++ Array(chr.head)

  def colored(cycle: (Array[Int])) = for(i <- 1 until cycle.length/2) yield (cycle(2*i-1), cycle(2*i))

  // Find the Colored Edges in a genome.
  def coloredEdges(dna: Array[Array[Int]]): Array[(Int,Int)] = {
    val circdna = dna.map(makeCircular)
    circdna.flatMap(chr => colored(chromosomeToCycle(chr)))
  }
}
