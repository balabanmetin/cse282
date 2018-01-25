package tasks

import tasks.Task24._
import tasks.Task27._
import tasks.Task29._
import tools.Solver


object Task30 extends Solver {

  import tools.Solver._

  override def solve(reader: Iterator[String]): Any = {
    val k = reader.nextInt
    val kmers = reader.map(_.trim).toList
    reconstructFromComposition(kmers)
  }

  // Reconstruct a string from its k-mer composition
  def reconstructFromComposition(kmers: List[String]): String = {
    reconstructFromGenomePath(eulerianPath(debruijnFromKmers(kmers)))
  }
}
