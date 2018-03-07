package tasks

import tools.Solver

object Task71 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val text = reader.trimmedLine
    val q = reader.nextInt
    lastToFirst(text)(q)
  }

  def lastToFirst(text: String): Int => Int = {
    val count = text.sorted.zipWithIndex.reverse.toMap
    val occurence = text.scanLeft(count.mapValues(i => 0)) { case (occ, c) =>
      occ + (c -> (occ.getOrElse(c, 0) + 1))
    }.tail
    (i: Int) => {
      val c = text.charAt(i)
      count.get(c).flatMap(ci => occurence(i).get(c).map(_ + ci - 1)).getOrElse(-1)
    }
  }
}
