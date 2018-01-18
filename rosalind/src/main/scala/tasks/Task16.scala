package tasks

import tools.Solver

object Task16 extends Solver {

  import Solver._

  def patternProbability(pattern: String, profile: Array[Array[Double]]): Double = {
    (0 until pattern.length).map(i => profile(nucToNum(pattern(i)))(i)).product
  }
  // Find a Profile-most probable k-mer in a string.
  def profileMostProbable(text: String, k: Int, profile: Array[Array[Double]]): String = {
    val probs = text.sliding(k).map(patternProbability(_, profile))
    text.sliding(k).zip(probs).maxBy(_._2)._1
  }

  override def solve(reader: Iterator[String]): Any = {
    val text = reader.trimmedLine
    val k = reader.nextInt
    val profile = reader.nextDoubleGrid(4)
    profileMostProbable(text, k, profile)
  }
}
