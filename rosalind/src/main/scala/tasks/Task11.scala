package tasks

import tools.Solver

object Task11 extends Solver {

  import Solver._

  def allkmers(k: Int, alphabet: List[String]): List[String] = {
    if (k == 1)
      alphabet
    else {
      allkmers(k - 1, alphabet).flatMap(s => alphabet.map(_ + s))
    }
  }

  //Generate the frequency array of a DNA string.
  def frequencyArray(seq: String, k: Int): List[Int] = {
    val theMap = seq.sliding(k).toList.groupBy(identity).mapValues(_.length)
    allkmers(k, List("A", "C", "G", "T")).sorted.map(m => theMap.getOrElse(m, 0))
  }

  override def solve(reader: Iterator[String]): Any = {
    val seq = reader.trimmedLine
    val k = reader.nextInt
    frequencyArray(seq, k).mkString(" ")
  }
}
