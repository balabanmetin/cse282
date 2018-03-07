package tasks

import tools.Solver

object Task72 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val text = reader.trimmedLine
    val patterns = reader.trimmedLine.split(" ")
    val count = (c: Char) => text.sorted.zipWithIndex.reverse.toMap.getOrElse(c, -1)
    val occurence = (c: Char, i: Int ) => {
      val seq = text.scanLeft(text.map(chr => chr -> 0).toMap) { case (occ, chr) =>
        occ + (chr -> (occ.getOrElse(chr, 0) + 1))
      }
      seq(i).getOrElse(c, -1)
    }
    patterns.map(pattern => betterBWTMatching(occurence, text, pattern, count)).mkString(" ")
  }

  def betterBWTMatching(occurence: (Char, Int) => Int,
                        text: String,
                        pattern: String,
                        count: Char => Int): Int = {
    var revpat = pattern.reverse
    var top = 0
    var bottom = text.length - 1
    while (top <= bottom){
      if(!revpat.isEmpty){
        val c = revpat.head
        revpat = revpat.tail
        if (text.substring(top, bottom + 1).contains(c)){
          top = count(c) + occurence(c, top)
          bottom = count(c) + occurence(c, bottom + 1) - 1
        } else {
          return 0
        }
      } else {
        return bottom - top + 1
      }
    }
    -1
  }
}
