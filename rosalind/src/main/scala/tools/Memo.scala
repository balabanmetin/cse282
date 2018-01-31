package tools

/** Author and credit to Pathikrit Bhowmick. See https://github.com/pathikrit/scalgos/
 */
/**
  * Generic way to create memoized functions (even recursive and multiple-arg ones)
  * @see http://stackoverflow.com/questions/25129721/ for full explanation of this
  *
  * @param f the function to memoize
  * @tparam I input to f
  * @tparam K the keys we should use in cache instead of I
  * @tparam O output of f
  */
case class Memo[I, K, O](f: I => O)(implicit ev: I => K) extends (I => O) {
  import scala.collection.mutable.{Map => Dict}
  val cache: Dict[K, O] = Dict.empty[K, O]
  override def apply(x: I): O = cache getOrElseUpdate (x, f(x))
}

object Memo {
  /**
    * Type of a simple memoized function e.g. when I = K
    */
  type ==>[I, O] = Memo[I, I, O]
}