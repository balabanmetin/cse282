package tasks

import tools.{Solver, SuffixTrie}

object Task63 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val text = reader.trimmedLine
    val trie = SuffixTrie(text)
    trie.nonBranchingPaths().mkString("\n")
  }
}
