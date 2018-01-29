package tasks

import tasks.Task24._
import tasks.Task27._
import tasks.Task35._
import tools.Solver

object Task33 extends Solver {

  override def solve(reader: Iterator[String]): Any = {
    val kmers = reader.map(_.trim).toList
    generateContigs(kmers).sorted.mkString("\n")
  }

  def generateContigs(kmers: List[String]): List[String] = {
    maximalNonBranchingPaths(debruijnFromKmers(kmers)).map(reconstructFromGenomePath)
  }

}
