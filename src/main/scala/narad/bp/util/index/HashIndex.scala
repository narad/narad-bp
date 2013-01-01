package narad.bp.util.index

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 12/27/12
 * Time: 12:24 AM
 * To change this template use File | Settings | File Templates.
 */
class HashIndex(n: Int=1000000000) extends Index[String] {

  def index(s: String): Int = {
    val h = s.hashCode
    if (h < n) h else scala.math.abs(h % n)
  }

  def indexOf(s: String) = index(s)
}
