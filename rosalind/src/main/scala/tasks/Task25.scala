package tasks

import tools.Solver

object Task25 extends Solver {

  import Solver._

  // Generate the k-mer composition of a string.
  def overlapGraph(dna: List[String]): List[(String, String)] = {
    val pairs = for(x <- dna; y <- dna) yield (x, y)
    pairs.filter{ case (x,y) =>
      x.tail == y.reverse.tail.reverse
    }
  }

  override def solve(reader: Iterator[String]): Any = {
    val dna = reader.map(_.trim).toList
    overlapGraph(dna).sorted.map{case (x,y) => x ++ " -> " ++ y}.mkString("\n")
  }
}
