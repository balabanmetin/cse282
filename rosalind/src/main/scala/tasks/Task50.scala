package tasks

import tools.Solver


object Task50 extends Solver {

  import tools.Solver._

  def readChromosome(reader: Iterator[String]): Array[Int] = {
    reader.trimmedLine.drop(1).dropRight(1).split("\\s+").map(_.toInt)
  }

  def chromosomeToString(array: Array[Int]): String = {
    def putPlus(i: Int): String = {
      if(i>0)
        s"+$i"
      else
        s"$i"
    }
    "(" ++ array.map(putPlus).mkString(" ") ++ ")"
  }

  def greedySortSteps(chr: Array[Int]): List[Array[Int]] = {
    _greedySortSteps(chr, 1)
  }

  def _greedySortSteps(chr: Array[Int], i: Int): List[Array[Int]] = {
    if(i == chr.length + 1)
      List.empty[Array[Int]]
    else {
      val ind = Math.max(chr.indexOf(i), chr.indexOf(-i))
      val newchr = chr.slice(0, i - 1) ++ chr.slice(i - 1, ind + 1).reverse.map(_ * -1) ++ chr.slice(ind + 1, chr.length)
      if(ind == chr.indexOf(-i))
        newchr :: _greedySortSteps(newchr, i + 1)
      else{
        val swapped = newchr.slice(0, i - 1) ++ newchr.slice(i - 1, i).map(_ * -1) ++ newchr.slice(i, chr.length)
        newchr :: swapped :: _greedySortSteps(swapped, i + 1)
      }
    }
  }

  override def solve(reader: Iterator[String]): Any = {
    val chr = readChromosome(reader)
    greedySortSteps(chr).map(chromosomeToString).mkString("\n")
  }

}
