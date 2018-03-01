package tasks

import tools.{PrefixTrie, Solver}

object Task62 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val seqs = reader.map(_.trim).toList
    val trie = PrefixTrie()
    seqs.foreach(trie.append)
    trie.toString
  }
}
