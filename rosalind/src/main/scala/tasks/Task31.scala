package tasks

import tasks.Task11._
import tasks.Task24._
import tasks.Task27._
import tasks.Task28._
import tools.Solver


object Task31 extends Solver {

  import tools.Solver._

  override def solve(reader: Iterator[String]): Any = {
    val k = reader.nextInt
    kUniversal(k)
  }

  // Find a k-universal circular binary string.
  def kUniversal(k: Int): String = {
    val kmers = allkmers(k, List("0", "1"))
    reconstructFromGenomePath(eulerianCycle(debruijnFromKmers(kmers))).drop(k-1)
  }
}
