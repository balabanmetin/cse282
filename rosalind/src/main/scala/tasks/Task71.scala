package tasks

import tools.Solver

object Task71 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val text = reader.trimmedLine
    val q = reader.nextInt
    lastToFirst(text)(text.charAt(q), q)
  }

  def lastToFirst(text: String): (Char, Int) => Int = {
    val count = text.sorted.zipWithIndex.reverse.toMap
    val occurence = text.scanLeft(count.mapValues(i => 0)) { case (occ, c) =>
      occ + (c -> (occ.getOrElse(c, 0) + 1))
    }
    (c: Char, i: Int) => count.get(c).flatMap(ci => occurence(i).get(c).map(_ + ci)).getOrElse(-1)
  }
}
