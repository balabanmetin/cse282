package tasks

import tools.{Solver, Trie}

object Task62 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val seqs = reader.map(_.trim).toList
    val trie = Trie()
    seqs.foreach(trie.append)
    trie.toString
  }
}
