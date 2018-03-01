package tools

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Created by metin on 2/2/18.
  */
object SuffixTrie {
  def apply(key: String) : Trie = {
    val trie = new TrieNode()
    for(i <- key.indices) trie.append(key.substring(i))
    trie
  }
}