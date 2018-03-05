package tasks

import tasks.Task67._
import tools.Solver

object Task69 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val text = reader.trimmedLine
    bwt(text)
  }

  def bwt(text: String): String = {
    def rotatedView(i: Int) = text.drop(i) ++ text.take(i)
    (for (i <- 0 until text.length) yield rotatedView(i)).sorted.map(_.takeRight(1)).mkString("")
  }
}
