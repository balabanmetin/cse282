package tasks

import tools.Solver
import tools.Memo

object Task36 extends Solver {

  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val target = reader.nextInt
    val coins = reader.trimmedLine.split(',').map(_.toInt).toList
    changeCount(coins, target)
  }

  // Find the minimum number of coins needed to make change.
  def changeCount(coins: List[Int], target: Int): Int = {
    type Input = (List[Int], Int)
    type Cache = (Int, Int)
    type Output = Int
    type DP = Memo[Input, Cache, Output]
    implicit def ev(key: Input): Cache = (key._1.length, key._2)

    lazy val f: DP = Memo {
      case (_, 0) => 0
      case (_, t) if t < 0 => Int.MaxValue / 2
      case (Nil, _) => Int.MaxValue / 2
      case (c :: cs, t) => Math.min(f(c :: cs, t - c) + 1, f(cs, t))
    }
    f(coins, target)
  }
}
