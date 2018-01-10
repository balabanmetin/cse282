package tasks

import com.sun.java.swing.plaf.motif.MotifTextUI.MotifCaret
import tools.Solver

object Task5 extends Solver {
  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val seq = reader.trimmedLine
    val params = reader.nextIntArray

    def occs(seq:String, motif:String): Iterator[Int]= {
      seq.sliding(motif.length).zipWithIndex.filter(_._1 == motif).map(_._2)
    }

    def occCount(seq:String,motif:String):Int = {
      seq.sliding(motif.length).count(_ == motif)
    }
    def checkClumps(seq:String, motif:String, L:Int, t:Int):Boolean = {
      val occuran = occs(seq,motif)
      occuran.sliding(t).exists(lst => (lst.size == t) && (lst.last - lst.head + 1) <= L)
    }

    seq.sliding(params(0)).filter(motif => checkClumps(seq,motif,params(1),params(2))).toList.distinct.mkString(" ")
  }
}
