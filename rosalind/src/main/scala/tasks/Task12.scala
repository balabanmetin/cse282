package tasks

import tools.Solver

object Task12 extends Solver {

  import Solver._

  //Convert a DNA string to a number.
  def hash(seq: String): Int = {
    val subtMap = Map('A' -> 0, 'C' -> 1, 'G' -> 2, 'T' -> 3)
    seq.reverse.foldLeft((0,0)){case ((acc,expon),c) =>
      (acc+Math.pow(4,expon).toInt*subtMap(c),expon+1)
    }._1
  }

  override def solve(reader: Iterator[String]): Any = {
    val seq = reader.trimmedLine
    hash(seq)
  }
}
