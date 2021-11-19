package tasks

import tools.Solver

object CountSingleton extends Solver {

  import tools.Solver._

  override def solve(reader: Iterator[String]): Any = {
    import scala.collection.mutable.{Map => Dict}
    val countMap = Dict.empty[List[Int], Int]
    reader.skipLines(1)
    while (reader.hasNext) {
      val Array(_, blah) = reader.trimmedLine.split(':')
      val blocks = blah.trim.split("\\s+").map(_.toInt)
      val signCorrect = (if(blocks.head < 0) blocks.reverse else blocks).toList
      val oldVal = countMap.getOrElse(signCorrect, 0)
      countMap(signCorrect) = oldVal + 1
    }
    countMap.groupBy(p => p._2).mapValues(_.size).toList.sorted
  }
}

