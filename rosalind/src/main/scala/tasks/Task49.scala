package tasks

import tools.Solver
import tools.Graph

object Task49 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val g = reader.nextGraphDS
    topologicalSort(g).mkString(", ")
  }

  // Find a topological ordering of a directed acyclic graph.
  def topologicalSort(g: Graph): List[String] = {
    def topologocalSort(in: Map[String, List[String]], sorted: List[String]): List[String] = {
      val (sinkNodes, others) = in.partition(_._2.isEmpty)
      if(sinkNodes.isEmpty) {
        if (others.isEmpty) sorted else sys.error("There are cycles\n" ++ others.toString())
      } else {
        val sinks = sinkNodes.keys.toList
        topologocalSort(others.mapValues(_.filter(s => !sinks.contains(s))), sorted ++ sinks)
      }
    }
    topologocalSort(g.in, List())
  }
}
