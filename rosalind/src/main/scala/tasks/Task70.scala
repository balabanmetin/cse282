package tasks

import tools.Solver

object Task70 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val text = reader.trimmedLine
    ibwt(text)
  }

  def ibwt(text: String): String = {
    def append(text: String, table: List[String]) = table.zip(text).map{case (s,c) => c +: s}
    var table = List.fill(text.length)("")
    for(i <- 0 until text.length) {
      table = append(text, table).sorted
    }
    table.head.tail + "$"
  }
}
