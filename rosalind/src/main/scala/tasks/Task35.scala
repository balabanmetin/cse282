package tasks

import tools.Solver


object Task35 extends Solver {

  import tools.Solver._

  override def solve(reader: Iterator[String]): Any = {
    val graph = reader.nextGraph
    val contigs = maximalNonBranchingPaths(graph)
    val cycles = findCycles(graph, contigs)
    (cycles ++ contigs).map(_.mkString("->")).mkString("\n")
  }

  // Find all maximal non-branching paths in a graph.
  def maximalNonBranchingPaths(graph: Map[String, List[String]]): List[List[String]] = {
    graph.flatMap{case (node, tail) =>
      val out = tail.length
      val in = graph.values.count(_.contains(node))
      if ((in != 1 || out != 1) && out > 0){
        tail.map(extractPath(graph, node, node, _))
      } else List.empty
    }.filter(_.length > 1).toList
  }

  def extractPath(graph: Map[String, List[String]], source: String, parent: String, current: String): List[String] = {
    val is1in1out = graph.get(current).exists(_.length == 1) && graph.count(_._2.contains(current)) == 1
    val tail = if (is1in1out && source != current)
      extractPath(graph, source, current, graph(current).head) else List(current)
    parent +: tail
  }

  def findCycles(graph: Map[String, List[String]], paths: List[List[String]]): List[List[String]] = {
    var cycgraph = graph.filter(_._2.length == 1).filter{case (node, tail) =>
      !paths.flatten.contains(node)
    }
    val cycles = cycgraph.map{case (node, tail) => extractPath(graph, node, node, tail.head) }
    cycles.foldLeft((List.empty[List[String]], Set.empty[String])){ case ((accPaths, accNodes), path) =>
      if (accNodes.contains(path.head)) (accPaths, accNodes)
      else (path +: accPaths, accNodes ++ path.toSet)
    }._1
  }
}
