package tasks

import tools.{Solver, SuffixTrie}

object Task66 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val text1 = reader.trimmedLine
    val text2 = reader.trimmedLine
    val concatText = text1+"#"+text2+"$"
    val trie = SuffixTrie(concatText)
    trie.shortestNonshared(concatText)
  }
}
