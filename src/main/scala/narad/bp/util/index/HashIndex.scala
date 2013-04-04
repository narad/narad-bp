package narad.bp.util.index
import scala.math._

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 12/27/12
 * Time: 12:24 AM
 * To change this template use File | Settings | File Templates.
 */
class HashIndex(n: Int=1000000000) extends Index[String] {

  def index(s: String): Int = {
    val h = abs(s.hashCode)+1
    if (h < n) h else (h % n)+1
  }

  def indexOf(s: String) = index(s)

  def size = -1
}
