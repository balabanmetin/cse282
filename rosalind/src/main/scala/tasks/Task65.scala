package tasks

import tools.{Solver, SuffixTrie}

object Task65 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val text1 = reader.trimmedLine
    val text2 = reader.trimmedLine
    val trie = SuffixTrie(text1+"#"+text2+"$")
    trie.longestRepeat()
  }
}
