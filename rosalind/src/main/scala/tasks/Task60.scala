package tasks

import tasks.Task50._
import tasks.Task55._
import tasks.Task56._
import tasks.Task57._
import tasks.Task58._
import tasks.Task59._
import tools.Solver

object Task60 extends Solver {

  import tools.Solver._

  override def solve(reader: Iterator[String]): Any = {
    val chr = readChromosome(reader)
    val Array(i1, i2, j1, j2) = reader.trimmedLine.split(", ").map(_.toInt)
    val colored = coloredEdges(Array(chr))
    // val black = blackEdges(Array(chr))
    val afterdcj = singleDCJop(colored.toList, i1, i2, j1, j2)
    graphToGenome(afterdcj).map(chromosomeToString).mkString(" ")
  }
}
