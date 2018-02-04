package tools

/**
  * Created by metin on 2/2/18.
  */
case class Graph(in: Map[String, List[String]], out: Map[String, List[String]]) {

}
object Graph {
  def fromOut(out: Map[String, List[String]]): Graph = {
    val nodes = out.values.flatten.toList.distinct
    val in = nodes.map(n => (n, out.filter(_._2.contains(n)).keys.toList)).toMap
    Graph(in, out)
  }
}
