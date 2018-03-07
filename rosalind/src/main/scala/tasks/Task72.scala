package tasks

import tools.Solver
import tasks.Task71._

object Task72 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val text = reader.trimmedLine
    val patterns = reader.trimmedLine.split(" ")
    val LF = lastToFirst(text)
    patterns.map(pattern => betterBWTMatching(text, pattern, LF)).mkString(" ")
  }

  def betterBWTMatching(text: String, pattern: String, LF: (Char, Int) => Int): Int = {
    var revpat = pattern.reverse
    var top = 0
    var bottom = text.length - 1
    while (top <= bottom){
      if(!revpat.isEmpty){
        val c = revpat.head
        revpat = revpat.tail
        if (text.substring(top, bottom + 1).contains(c)){
          top = LF(c, top)
          bottom = LF(c, bottom + 1) - 1
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
