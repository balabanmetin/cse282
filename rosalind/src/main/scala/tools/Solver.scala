package tools

import java.io.{File, FileWriter}
import java.util.regex.Pattern
import scala.io.Source
import scala.math._


abstract class Solver {
  def solve(reader: Iterator[String]): Any

  def name = ""

  def main(args: Array[String]) {
    val input: String = "data.txt"
    val iterator: Iterator[String] = Source.fromResource(input).getLines()
    val file = new File(input.substring(0, input.lastIndexOf(".")) + ".out")
    val results = List(solve(iterator))
    val fw = new FileWriter(file)
    try {
      fw.write(results.mkString("\n"))
    } finally {
      fw.close()
    }
  }
}

object Solver {
  case class Point(x: Int, y: Int) {
    def dist(p: Point) = sqrt(pow(x - p.x, 2) + pow(y - p.y, 2))
  }
  case class FPoint(x: Double, y: Double) {
    def dist(p: FPoint) = sqrt(pow(x - p.x, 2) + pow(y - p.y, 2))
  }

  val nucToNum = Map('A' -> 0, 'C' -> 1, 'G' -> 2, 'T' -> 3)

  class RichIterator(val iterator: Iterator[String]) {

    def trimmedLine = iterator.next().trim()
    def nextInt: Int = trimmedLine.toInt
    def nextIntArray: Array[Int] = nextStringArray map (_.toInt)
    def nextCharArray: Array[Char] = iterator.next().toCharArray
    def nextLongArray: Array[Long] = nextStringArray map (_.toLong)
    def nextDoubleArray: Array[Double] = nextStringArray map (_.toDouble)
    def nextBigDecimalArray: Array[BigDecimal] = nextStringArray map (BigDecimal(_))
    def nextBigIntArray: Array[BigInt] = nextStringArray map (BigInt(_))
    def nextStringArray: Array[String] = trimmedLine split "\\s+"
    def nextIntGrid(y: Int): Array[Array[Int]] = (for (i <- 0 until y) yield nextIntArray).toArray
    def nextDoubleGrid(y: Int): Array[Array[Double]] = (for (i <- 0 until y) yield nextDoubleArray).toArray
    def nextCharGrid(y: Int): Array[Array[Char]] = (for (i <- 0 until y) yield nextCharArray).toArray
    def nextGraph: Map[String, List[String]] = {
      iterator.map(_.trim).map(_.split(" -> ")).map(pair => (pair(0), pair(1).split(",").toList)).toMap
    }
    def nextGraphDS: Graph = {
      val g = nextGraph
      val nodes = (g.keys ++ g.values.flatten).toList.distinct
      val out = nodes.map(n => (n, g.getOrElse(n, List()))).toMap
      val in  = nodes.map(n => (n, out.filter(_._2.contains(n)).keys.toList)).toMap
      Graph(in, out)
    }

    def skipLines(nr: Int) {
      for (i <- 1 to nr) {
        iterator.next()
      }
    }
  }

  implicit def iteratorToHelper(x: Iterator[String]): RichIterator = new RichIterator(x)
}

