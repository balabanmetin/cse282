package tasks

import tools.Solver

object Task28 extends Solver {

  import tools.Solver._

  override def solve(reader: Iterator[String]): Any = {
    val graph = reader.nextGraph
    eulerianCycle(graph).mkString("->")
  }

  case class View(graph: Map[String, List[String]], path: List[String])

  // Find an Eulerian cycle in a graph.
  def eulerianCycle(graph: Map[String, List[String]]): List[String] = {
    _eulerian(graph.head._1, View(graph ,List.empty)).path
  }

  // helper
  def _eulerian(current: String, view: View): View= {
    var curView = view
    while(curView.graph(current) != List.empty)
      curView = _eulerian(curView.graph(current).head,
                          View(curView.graph + (current -> curView.graph(current).tail), curView.path))
    View(curView.graph, current +: curView.path)
  }
}
