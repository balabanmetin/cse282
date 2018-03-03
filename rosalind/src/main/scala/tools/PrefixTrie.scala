package tools

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Created by metin on 2/2/18.
  */
object PrefixTrie {
  def apply() : Trie = new TrieNode()
}

sealed trait Trie {

  def append(key : String)
  def prefixTreeMatching(prefix: String): Boolean
  def nonBranchingPaths(): List[String]
  def longestRepeat(): String
  def setColor()
  def longestShared(): String
}

private[tools] class TrieNode(val char : Option[Char] = None,
                              var word: Option[String] = None,
                              var color: Int = 0) extends Trie {

  private[tools] val children: mutable.Map[Char, TrieNode] = new mutable.TreeMap[Char, TrieNode]()

  override def append(key: String): Unit = {

    @tailrec def appendHelper(node: TrieNode, currentIndex: Int): Unit = {
      if (currentIndex == key.length) {
        node.word = Some(key)
      } else {
        val char = key.charAt(currentIndex)
        val result = node.children.getOrElseUpdate(char, {
          new TrieNode(Some(char))
        })

        appendHelper(result, currentIndex + 1)
      }
    }

    appendHelper(this, 0)
  }

  override def prefixTreeMatching(prefix: String): Boolean = {

    @tailrec def helper(currentIndex: Int, node: TrieNode): Boolean = {
      if (node.word.isDefined) {
        true
      } else if (currentIndex == prefix.length) {
        false
      } else {
        node.children.get(prefix.charAt(currentIndex)) match {
          case Some(child) => helper(currentIndex + 1, child)
          case None => false
        }
      }
    }

    helper(0, this)
  }

  override def nonBranchingPaths(): List[String] = {

    def helper(incoming: String, node: TrieNode): List[String] = {
      if (node.children.isEmpty) {
        List(incoming)
      } else if(node.children.size == 1) {
        val (chr, child) = node.children.head
        val s::ss = helper(chr.toString, child)
        (incoming + s) :: ss
      } else {
        incoming.toString :: node.children.flatMap{case (chr, child) => helper(chr.toString, child)}.toList
      }
    }

    helper("", this).tail
  }

  override def longestRepeat(): String = {

    def helper(path: String, longest: String, node:TrieNode): String = {
      val newLong =
        if(path.length > longest.length && node.children.size > 1) path else longest
      node.children.foldLeft(newLong){case (lng,(chr, n)) =>
        helper(path+chr, lng, n)
      }
    }

    helper("", "", this)
  }

  override def setColor(): Unit = {

    def helper(node: TrieNode): Unit = {
      if (node.children.isEmpty){
        if (node.word.get.contains("#")) node.color = 1 else node.color = 2
      } else {
        node.children.values.foreach(helper)
        if (node.children.values.forall(_.color == 1))
          node.color = 1
        else if (node.children.values.forall(_.color == 2))
          node.color = 2
        else
          node.color = 3
      }
    }

    helper(this)
  }

  override def longestShared(): String = {
    this.setColor()

    def helper(path: String, longest: String, node:TrieNode): String = {
      val newLong =
        if(path.length > longest.length && node.children.size > 1 && node.color == 3) path else longest
      node.children.foldLeft(newLong){case (lng,(chr, n)) =>
        helper(path+chr, lng, n)
      }
    }

    helper("", "", this)
  }


  override def toString() : String = {

    def helper(currentIndex: Int, node: TrieNode): Iterable[String] = {
      val adj = node.children.zipWithIndex.map { case ((c, _), i) => s"$currentIndex->${currentIndex + i + 1}:$c" }
      adj ++ node.children.zipWithIndex.flatMap{ case ((_, next), i) => helper(currentIndex + i + 1, next) }
    }

    helper(0, this).mkString("\n")
  }

}