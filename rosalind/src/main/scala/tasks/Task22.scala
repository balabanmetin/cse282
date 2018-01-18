package tasks

import tools.Solver

object Task22 extends Solver {

  import Solver._

  //Convert a number to a DNA string.
  def unhash(i: Long, k: Int): String = {
    val subtMap = Map('0' ->'A', '1' -> 'C', '2' -> 'G', '3' -> 'T')
    val tail = java.lang.Long.toString(i, 4).map(subtMap(_))
    "A"*(k - tail.length)+tail
  }

  override def solve(reader: Iterator[String]): Any = {
    val i = reader.nextLongArray(0)
    val k = reader.nextInt
    unhash(i, k)
  }
}
