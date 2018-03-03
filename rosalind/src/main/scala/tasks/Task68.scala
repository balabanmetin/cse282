package tasks

import tools.Solver
import tasks.Task67._

object Task68 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val text = reader.trimmedLine + "$"
    val patterns = reader.map(_.trim).toList
    val arr = suffixArray(text)
    val ranges = patterns.map(pattern => matchWithSuffixArray(pattern, text, arr.toArray))
    ranges.flatMap{case (a, b) => arr.slice(a, b)}.sorted.mkString(" ")
  }

  def matchWithSuffixArray(pattern: String, text: String, arr: Array[Int]): (Int, Int) = {
    var minIndex = 0
    var maxIndex = text.length
    while (minIndex < maxIndex) {
      val midIndex = (minIndex + maxIndex) / 2
      if (pattern > text.substring(arr(midIndex))) minIndex = midIndex + 1 else maxIndex = midIndex
    }
    val first = minIndex
    maxIndex = text.length
    while (minIndex < maxIndex) {
      val midIndex = (minIndex + maxIndex) / 2
      if (pattern + "~" < text.substring(arr(midIndex)))
        maxIndex = midIndex
      else
        minIndex = midIndex + 1
    }
    val last = maxIndex
    if (first > last)
      (-1, -1)
    else
      (first, last)
  }
}
