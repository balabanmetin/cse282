package tasks

import tools.{Solver, SuffixTrie}

object Task67 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val text = reader.trimmedLine
    suffixArray(text).mkString(", ")
  }
  def suffixArray(text: String): List[Int] = {
    val suffixes = (for(i <- text.indices) yield text.substring(i)).zipWithIndex
    suffixes.sortBy(_._1).map(_._2).toList
  }
}
