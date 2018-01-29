package tasks


import tools.Solver
import tasks.Task24._


object Task34 extends Solver {

  import tools.Solver._

  override def solve(reader: Iterator[String]): Any = {
    val Array(k,d) = reader.nextIntArray
    val pairedKmers = reader.map(_.trim).toList.map(_.split('|').toList)
    pairedEndGenomePath(pairedKmers, k, d)
  }

  def pairedEndGenomePath(pairedKmers: List[List[String]], k: Int, d: Int): String = {
    val List(firstSet, secondSet) = pairedKmers.transpose
    reconstructFromGenomePath(firstSet) ++ reconstructFromGenomePath(secondSet).takeRight(k + d)
  }
}
