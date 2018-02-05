package tasks

import tools.{Memo, Solver}

object Task37 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val Array(n, m) = reader.nextIntArray
    val down = reader.nextIntGrid(n)
    reader.skipLines(1)
    val right = reader.nextIntGrid(n + 1)
    manhattanLongestPath(n, m, down, right)
  }

  // Find the length of a longest path in a rectangular city.
  def manhattanLongestPath(n: Int, m: Int, down: Array[Array[Int]], right: Array[Array[Int]]): Int = {
    type Input = (Int, Int)
    type Cache = (Int, Int)
    type Output = Int
    type DP = Memo[Input, Cache, Output]

    lazy val f: DP = Memo {
      case (0, 0) => 0
      case (a, 0) => f(a - 1, 0) + down(a - 1)(0)
      case (0, b) => f(0, b - 1) + right(0)(b - 1)
      case (a, b) => List(f(a - 1, b) + down(a - 1)(b),
                          f(a, b - 1) + right(a)(b - 1)).max
    }
    f(n, m)
  }
}
