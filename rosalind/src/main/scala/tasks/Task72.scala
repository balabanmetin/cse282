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
    val (top, bottom) = pattern.reverse.foldLeft((0, text.length - 1)) { case ((t, b), c) =>
      if (text.substring(t, b + 1).contains(c)) (LF(c, t), LF(c, b + 1) - 1) else (0, -1)
    }
    bottom - top + 1
  }
}
