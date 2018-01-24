package tasks

import tools.Solver

object Task24 extends Solver {

  override def solve(reader: Iterator[String]): Any = {
    val dna = reader.map(_.trim).toList
    reconstructFromGenomePath(dna)
  }

  // Find the string spelled by a genome path.
  def reconstructFromGenomePath(path: List[String]): String = {
    path.head ++ path.tail.map(_.last)
  }
}
