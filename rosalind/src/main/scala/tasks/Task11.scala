package tasks

import tools.Solver

object Task11 extends Solver {

  import Solver._

  def allkmers(k: Int): List[String] = {
    if (k == 1)
      List("A", "C", "G", "T")
    else {
      allkmers(k - 1).flatMap(s => List("A", "C", "G", "T").map(_ + s))
    }
  }

  //Generate the frequency array of a DNA string.
  def frequencyArray(seq: String, k: Int): List[Int] = {
    val theMap = seq.sliding(k).toList.groupBy(identity).mapValues(_.length)
    allkmers(k).sorted.map(m => theMap.getOrElse(m, 0))
  }

  override def solve(reader: Iterator[String]): Any = {
    val seq = reader.trimmedLine
    val k = reader.nextInt
    frequencyArray(seq, k).mkString(" ")
  }
}
