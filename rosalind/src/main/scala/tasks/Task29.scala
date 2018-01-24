package tasks

import tools.Solver
import tasks.Task28._

object Task29 extends Solver {

  import tools.Solver._

  override def solve(reader: Iterator[String]): Any = {
    val graph = reader.nextGraph
    eulerianPath(graph).mkString("->")
  }

  def balanceCount(node: String, graph: Map[String, List[String]]): Int = {
    val out = graph(node).length
    val in = graph.values.count(_.contains(node))
    out - in
  }

  // Find an Eulerian cycle in a graph.
  def eulerianPath(graph: Map[String, List[String]]): List[String] = {
    val source = graph.keys.filter(key => balanceCount(key, graph) % 2 == 1).head
    _eulerian(source, View(graph ,List.empty)).path
  }
}
