package tasks

import tools.{PrefixTrie, Solver}

object Task61 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val text = reader.trimmedLine
    val seqs = reader.map(_.trim).toList
    val trie = PrefixTrie()
    seqs.foreach(trie.append)
    val arr = (for (i <- text.indices) yield if(trie.prefixTreeMatching(text.substring(i))) Some(i) else None).flatten
    arr.mkString(" ")
  }
}
