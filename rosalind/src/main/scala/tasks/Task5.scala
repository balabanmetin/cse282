package tasks

import com.sun.java.swing.plaf.motif.MotifTextUI.MotifCaret
import tools.Solver

object Task5 extends Solver {
  import Solver._

  override def solve(reader: Iterator[String]): Any = {
    val seq = reader.trimmedLine
    val params = reader.nextIntArray

    def occCount(seq:String,motif:String):Int = {
      seq.sliding(motif.size).count(_ == motif)
    }
    def checkClumps(seq:String, motif:String, L:Int, t:Int):Boolean = {
      seq.sliding(L).exists(LString => occCount(LString,motif)>=t)
    }

    seq.sliding(params(0)).filter(motif => checkClumps(seq,motif,params(1),params(2))).toList.distinct.mkString(" ")
  }
}
