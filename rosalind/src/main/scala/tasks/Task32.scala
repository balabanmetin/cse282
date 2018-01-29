package tasks

import tasks.Task30._
import tools.Solver


object Task32 extends Solver {

  import tools.Solver._

  override def solve(reader: Iterator[String]): Any = {
    val Array(k,d) = reader.nextIntArray
    val pairedKmers = reader.map(_.trim).toList.map(_.split('|').toList)
    pairedEndReconstruction(pairedKmers, k, d)
  }

  // paired-end assembly yaklasik cozum
  def pairedEndReconstruction(pairedKmers: List[List[String]], k: Int, d: Int): String = {
    val List(firstSet, secondSet) = pairedKmers.transpose
    reconstructFromComposition(firstSet) ++ reconstructFromComposition(secondSet).takeRight(k + d)
  }
}
