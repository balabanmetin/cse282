package tasks

import tools.Solver

object CopyNumberDist extends Solver {

  import tools.Solver._

  override def solve(reader: Iterator[String]): Any = {
    import scala.collection.mutable.{Map => Dict}
    val countMap = Dict.empty[String, Int]
    reader.skipLines(1)
    while (reader.hasNext) {
      val Array(_, _, _, _, _, _, blah) = reader.trimmedLine.split("\\s+")
      val oldVal = countMap.getOrElse(blah, 0)
      countMap(blah) = oldVal + 1
    }
    countMap.count(p => p._2 == 1)
  }
}

